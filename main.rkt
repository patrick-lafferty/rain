#lang racket

;Basic shell that uses Racket as its scripting language

(require "shell.rkt")
(require racket/runtime-path)

(define-runtime-path builtins "builtins.rkt")

(define prompt-character 
    (match (system-type 'os)
        ['unix "λ "]
        ['windows "> "]
        ['macosx "λ "]
    ))

(require ffi/unsafe)
(require ffi/unsafe/define)
(define-ffi-definer define-libc (ffi-lib #f))

(define-libc signal (_fun _int _int -> _void))

(define SIGTTOU 22)
(signal 2 1)
(signal SIGTTOU 1)

(require "ffi_readline.rkt")
(define-libc getchar (_fun -> _int))

(define safe-read-line
    (match (system-type 'os)
        ['unix ffi-read-line]
        [_ read-line]))

;up: 27, 91, 65
;left: 27, 91, 68
;right: 27, 91, 67
;down: 27, 91, 66
;f1: 27, 79, 80
;backspace: 127
;return: 10
;del: 27, 91, 51, 126
;tab: 9

(define (refresh-line [show-prompt? #t])
    (printf "\x1b[2K") ;ANSI escape code CSI n K - Erase in Line
    (printf "\x1b[1G") ;ANSI escape code CSI n G - Cursor Horizontal Absolute
    (when show-prompt?
        (display prompt-character))
    (display (send commandline get-line-single))
    (printf "\x1b[~aG" (+ (if show-prompt? 3 1) (send commandline get-position)))
    (flush-output)
    )

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
        (refresh-line)))

(define (handle-escape-sequence)
    (match (parse-escape-sequence)
        ['f1 (display "f1")]
        ['del (send commandline delete) (refresh-line)]
        ['up 
            (show-history up-counter)
            (when (< up-counter (send history get-length))
                (set! up-counter (+ up-counter 1)))]
        ['down 
            (when (> up-counter -1)
                (set! up-counter (- up-counter 1)))
            (show-history up-counter)]
        ['right (send commandline move-right)]
        ['left (send commandline move-left)]
        ['unsupported (display "unsupported")]))

(require "commandline.rkt")
(define commandline (new commandline%))

(require "history.rkt")
(define history (new history%))

;(define (a) (ls))

;balance define(a)(ls) [)]
;...
;(a) (ls)) [)]
;a) (ls)) [))]

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


(define (input-loop [show-prompt? #t])
    (with-handlers ([exn:fail? (lambda (e) (displayln e))])
    (let ([c (getchar)])
        (match c
            [4 (send commandline clear)]
            [9 (input-loop show-prompt?)]
            [10 (displayln "")   
                (set! up-counter 0)

                (if (> (send commandline get-length) 0)
                    (let ([line (send commandline get-line)])
                        (if (balanced line)
                            (begin 
                                (send history add line)
                                (let ([code (read (open-input-string line))])                     
                                    (cond
                                        [(list? code) (exec code)]
                                        [(symbol? code) (handle-symbol code)]
                                        [ else (printf "unknown: ~a~n" code)]))
                                (send commandline clear))
                            (begin 
                                (send commandline store)
                                (send commandline clear-single)
                                (refresh-line #f)
                                (input-loop #f))))
                    (when (send commandline is-in-multiline?)
                        (refresh-line #f)
                        (input-loop #f)))]
            [27 (handle-escape-sequence) (input-loop show-prompt?)]
            [127 (send commandline backspace) (refresh-line show-prompt?) (input-loop show-prompt?)]
            [(? negative?) (displayln "eof?")]
            [_ 
                (send commandline add-char (integer->char c))
                (refresh-line show-prompt?)
                (input-loop show-prompt?)]))))

(define (repl)
    (with-handlers
        ([exn:fail? (lambda (e) (displayln e))])
        (refresh-line)
        (flush-output)
        (input-loop)
        (repl)))

(repl)
