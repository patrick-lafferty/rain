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

(define-libc signal (_fun _int _int -> _void));(_fun _int -> _void) -> _void)))

(signal 22 1)

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

(define (refresh-line)
    (printf "\x1b[2K")
    (printf "\x1b[1G")
    (display prompt-character)
    (display (send commandline get-line))
    (printf "\x1b[~aG" (+ 3 (send commandline get-position)))
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

(define (handle-escape-sequence)
    (match (parse-escape-sequence)
        ['f1 (display "f1")]
        ['del (send commandline delete) (refresh-line)]
        ['up (display "up")]
        ['down (display "down")]
        ['right (send commandline move-right)]
        ['left (send commandline move-left)]
        ['unsupported (display "unsupported")]))

(require "commandline.rkt")
(define commandline (new commandline%))

(define (input-loop)
    (let ([c (getchar)])
        (match c
            [9 (input-loop)]
            [10 (displayln "")   
                (let ([code (read (open-input-string (send commandline get-line)))])
                    (cond
                        [(list? code) (exec code)]
                        [(symbol? code) (handle-symbol code)]
                        [ else (printf "unknown: ~a~n" code)]))
                (send commandline clear)]
            [27 (handle-escape-sequence) (input-loop)]
            [127 (send commandline backspace) (refresh-line) (input-loop)]
            [_ 
                (send commandline add-char (integer->char c))
                (display (integer->char c))
                (flush-output)
                (input-loop)])))


(define (repl)
    (display prompt-character)
    (flush-output)
    (input-loop)
    (repl))

(repl)
