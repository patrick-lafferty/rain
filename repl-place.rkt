#|
MIT License
Copyright (c) 2017 Patrick Lafferty
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
#lang racket/base

(require racket/match)
(require racket/class)
(require racket/list)

;Basic shell that uses Racket as its scripting language

(require racket/place)

(require ffi/unsafe)
(require ffi/unsafe/define)
(define-ffi-definer define-libc (ffi-lib #f))

(define-libc signal (_fun _int _int -> _void))

(define SIGTTOU 22)
(signal 2 1)
(signal SIGTTOU 1)

(require "ffi_readline.rkt")
(define-libc getchar (_fun -> _int))

(define (parse-escape-sequence)
    (let ([c1 (getchar)])
        (match c1
            [79 (let ([c2 (getchar)])
                    (match c2
                        [80 'f1]
                        [_ 'unsupported]))]
            [91 (let ([c2 (getchar)])
                    (match c2
                        [51 (let ([c3 (getchar)])
                                (match c3
                                    [126 'del]
                                    [_ 'unsupported]))]
                        [65 'up]
                        [66 'down]
                        [67 'right]
                        [68 'left]))])))

(define up-counter 0)

(define (show-history i)
    (let ([past-line (send history get i)])
        (send commandline set-from-history past-line) 
    ))
        ;(refresh-line)))

(define (handle-escape-sequence channel)
    (match (parse-escape-sequence)
        ['f1 (display "f1")]
        ['del (send commandline delete)] ;(refresh-line)]
        ['up 
            (show-history up-counter)
            (when (< up-counter (send history get-length))
                (set! up-counter (+ up-counter 1)))]
        ['down 
            (when (> up-counter -1)
                (set! up-counter (- up-counter 1)))
            (show-history up-counter)]
        ['right 
            (send commandline move-right)
            (place-channel-put channel (list 'update-cursor (send commandline get-position)))]
        ['left 
            (send commandline move-left)
            (place-channel-put channel (list 'update-cursor (send commandline get-position)))]
        ['unsupported (display "unsupported")]))

(require "commandline.rkt")
(define commandline (new commandline%))

(require "history.rkt")
(define history (new history%))

(define (balanced s)
    (define (opposite c)
        (match c
            [#\( #\)]
            [#\[ #\]]
            [#\{ #\}]))
            
    (define (helper str expected)
        (let ([c (if (list? str) (if (null? str) str (first str)) str)])
            (match c
                ['() (null? expected)]
                [(or #\( #\[ #\{) (helper (rest str) (cons (opposite c) expected))]
                [(or #\) #\] #\}) 
                    (if (eqv? c (first expected))
                        (helper (rest str) (rest expected))
                        #f)]
                [_ (helper (rest str) expected)])))

    (let ([str (if (string? s) (string->list s) s)])
        (helper str '())))

(require "sh-lang.rkt")

(define (input-loop channel [show-prompt? #t])
    (with-handlers ([exn:fail? (lambda (e) (displayln e))])
    (place-channel-put channel (list 'update-cursor (send commandline get-position)))
    (let ([c (getchar)])
        (match c
            [4 (send commandline clear)]
            [9 (input-loop channel show-prompt?)]
            [10 (displayln "")   
                (set! up-counter 0)

                (if (> (send commandline get-length) 0)
                    (let ([line (send commandline get-line)])
                        (if (balanced line)
                            (begin 
                                (send history add line);(list->string line))
                                (place-channel-put channel (list 'finished line));(send commandline get-line-single)));line))
                                (send commandline clear))
                            (begin 
                                (send commandline store)
                                (place-channel-put channel (list 'incomplete '())); (send commandline get-line-single)));line))
                                (send commandline clear-single)
                                ;(refresh-line #f)
                                (input-loop channel #f))))
                    (when (send commandline is-in-multiline?)
                        ;(refresh-line #f)
                        (place-channel-put channel (list 'incomplete (send commandline get-line-single)))
                        (input-loop channel #f)))]
            [27 
                (handle-escape-sequence channel) 
                (place-channel-put channel (list 'update #t (send commandline get-line-single)))
                (input-loop channel show-prompt?)]
            [127 
                (send commandline backspace) 
                ;(refresh-line show-prompt?) 
                (place-channel-put channel (list 'update show-prompt? (send commandline get-line-single)))
                (input-loop channel show-prompt?)]
            [(? negative?) (void)];(displayln "eof?")]
            [_ 
                (send commandline add-char (integer->char c))
                ;(refresh-line show-prompt?)
                (place-channel-put channel (list 'update show-prompt? (send commandline get-line-single)))
                (input-loop channel show-prompt?)]))))

(provide create-repl-place)

(define (repl channel)
    (with-handlers
        ([exn:fail? (lambda (e) (displayln e))])
        ;(refresh-line)
        ;(flush-output)
        (input-loop channel)
        (repl channel)))

(define (create-repl-place)
    (place channel (repl channel)))