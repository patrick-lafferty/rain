#lang racket

;Basic shell that uses Racket as its scripting language
;(require racket/rerequire)
;(dynamic-rerequire "shell.rkt")
;(require racket/rerequire)

;(define namespace (module->namespace "shell.rkt"))
;(define symbols (namespace-mapped-symbols namespace))

;(writeln (member 'test-1 symbols))
(require "shell.rkt")
(define prompt-character "> ")
;(define prompt-character "λ ")

;(define (reload-shell)
;    (dynamic-rerequire "shell.rkt"))

(define (handle-symbol s)
    (match s
        ;['reload (dynamic-rerequire "main.rkt")]
        ['reload (begin 
            (reload-shell)
            ;(set! namespace (module->namespace "shell.rkt"))
        )]
        
    )
    )

(define (repl)
    (display prompt-character)
    (let ([code (read)])
        (displayln code)
        (match code
            [(list* head tail) (exec code)]
            [symbol? (handle-symbol code)]
            [ _ (displayln "unknown")]
        )
        (repl)
        ;(eval code namespace)
    ))

(repl)
