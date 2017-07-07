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

(struct cursor-position (row column))
(struct control-sequence-introducer (code bytes))

(define (lex-csi acc)
    (let ([c (getchar)])
        (if (< c 64)
            (lex-csi (cons (integer->char c) acc))
            (control-sequence-introducer c (reverse acc)))))

(define (parse-csi)
    (let ([csi (lex-csi '())])
        (match (integer->char (control-sequence-introducer-code csi))
            ;CUU - cursor up 
            [#\A 'up]
            ;CUD - cursor down
            [#\B 'down]
            ;CUF - cursor forward
            [#\C 'right]
            ;CUB - cursor back
            [#\D 'left]
            [#\~ 'del]
            ;CNL - cursor next line
            #|[#\E]
            ;CPL - cursor previous line
            [#\F]
            ;CHA - cursor horizontal absolute
            [#\G]
            ;CUP - cursor position
            [#\H]
            ;ED - erase in display
            [#\J]
            ;EL - erase in line
            [#\K]
            ;SU - scroll up
            [#\S]
            ;SD - scroll down
            [#\T]
            ;HVP - horizontal and vertical position
            [#\f]|#
            ;CPR - cursor position
            ;bytes: row... ; col...
            [#\R
                (let-values ([(row column) (splitf-at (control-sequence-introducer-bytes csi) 
                        (lambda (c) (not (eqv? c #\;))))])
                    (let ([row (string->number (list->string row))]
                          [column (string->number (list->string (rest column)))])
                            (cursor-position row column)))])))
            

(define (parse-escape-sequence)
    (let ([c1 (getchar)])
        (match c1
            ;79 is O
            [79 (let ([c2 (getchar)])
                    (match c2
                        ;f1 - f4 is ^[OP...^[OS
                        ;80 is P
                        [80 'f1]
                        [_ 'unsupported]))]
            ;91 is [
            [91 (parse-csi)])))

(define up-counter 0)

(define (show-history i)
    (let ([past-line (send history get i)])
        (send commandline set-from-history past-line) 
    ))

(define (handle-escape-sequence channel)
    (match (parse-escape-sequence)
        [(cursor-position row column)
            (place-channel-put channel (list 'cursor-position row column))
            #f]
        ['f1 (display "f1") #f]
        ['del (send commandline delete) #t] 
        ['up 
            (show-history up-counter)
            (when (< up-counter (send history get-length))
                (set! up-counter (+ up-counter 1)))
            #t]
        ['down 
            (when (> up-counter -1)
                (set! up-counter (- up-counter 1)))
            (show-history up-counter)
            #t]
        ['right 
            (send commandline move-right)
            (place-channel-put channel (list 'update-cursor (send commandline get-position)))
            #t]
        ['left 
            (send commandline move-left)
            (place-channel-put channel (list 'update-cursor (send commandline get-position)))
            #t]
        ['unsupported (void) #t]))

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
    ;(place-channel-put channel (list 'update-cursor (send commandline get-position)))
    (let ([c (getchar)])
        (match c
            ;EoF from user pressing Ctrl-D
            [4
                (send commandline clear)
                (place-channel-put channel 'clear)]
            [9 (input-loop channel show-prompt?)]
            ;newline from user pressing enter
            [10 (displayln "")   
                (set! up-counter 0)

                (if (> (send commandline get-length) 0)
                    (let ([line (send commandline get-line)])
                        (if (balanced line)
                            (begin 
                                (send history add line)
                                (place-channel-put channel (list 'finished line))
                                (send commandline clear))
                            (begin 
                                (send commandline store)
                                (place-channel-put channel (list 'incomplete '()))
                                (send commandline clear-single)
                                (input-loop channel #f))))
                    (when (send commandline is-in-multiline?)
                        (place-channel-put channel 'newline)
                        (input-loop channel #f)))]
            [27 
                (let ([update? (handle-escape-sequence channel)])
                    (when update?
                        (place-channel-put channel (list 'update show-prompt? (send commandline get-line-single))))
                    (input-loop channel show-prompt?))]
            [127 
                (send commandline backspace) 
                (place-channel-put channel (list 'update-cursor (send commandline get-position)))
                (place-channel-put channel (list 'update show-prompt? (send commandline get-line-single)))
                (input-loop channel show-prompt?)]
            [(? negative?) (void)]
            [_ 
                (send commandline add-char (integer->char c))
                (place-channel-put channel (list 'update-cursor (send commandline get-position)))
                (place-channel-put channel (list 'update show-prompt? (send commandline get-line-single)))
                (input-loop channel show-prompt?)]))))

(provide create-repl-place)

(define (repl channel)
    (with-handlers
        ([exn:fail? (lambda (e) (displayln e))])
        (input-loop channel)
        (repl channel)))

(define (setup channel)
    (repl channel))

(define (create-repl-place)
    (place channel (setup channel)))