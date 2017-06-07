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
(define libc (ffi-lib #f))
(define signal (get-ffi-obj "signal" libc (_fun _int _int -> _void)));(_fun _int -> _void) -> _void)))

(define (sigint i)
    (displayln "sigint"))

(define (sigtstp i)
    (displayln "sigtstp"))

;(signal 21 sigint)
(signal 22 1);sigtstp)

(require "ffi_readline.rkt")

(define safe-read-line
    (match (system-type 'os)
        ['unix ffi-read-line]
        [_ read-line]))

(define (repl)
    (display prompt-character)
    (flush-output)
    (let* ([line (safe-read-line)] 
           [code (read (open-input-string line))])
        (cond
            [(list? code) (exec code)]
            [(symbol? code) (handle-symbol code)]
            [ else (printf "unknown: ~a~n" code)])
        
        (repl)
    ))

(repl)
