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
