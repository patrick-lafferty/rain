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
(define signal (get-ffi-obj "signal" libc (_fun _int (_fun _int -> _void) -> _void)))

(define (sigint i)
    (display "sigint"))

(define (sigtstp i)
    (display "sigtstp"))

(signal 2 sigint)
(signal 20 sigtstp)

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
            [ else (displayln "unknown")])
        
        (repl)
    ))

(repl)
