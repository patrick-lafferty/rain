#lang racket

;Basic shell that uses Racket as its scripting language

(require "shell.rkt")
(require errortrace)

(define prompt-character 
    (match (system-type 'os)
        ['unix "λ "]
        ['windows "> "]
        ['macosx "λ "]
    ))

(define (repl)
    (display prompt-character)
    (let ([code (read)])
        (match code
            [(list* head tail) (exec code)]
            [symbol? (handle-symbol code)]
            [ _ (displayln "unknown")]
        )
        (repl)
    ))

(repl)
