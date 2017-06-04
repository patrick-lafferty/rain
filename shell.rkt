#lang racket

;This module defines the namespace to be used for
;the shell's REPL

(require racket/rerequire)
;(dynamic-rerequire "builtins.rkt" )
;(require "sig.rkt")

;(define-namespace-anchor a)
;(define namespace (module->namespace "builtins.rkt"))
;(define-namespace-anchor a)

(define (combine-namespaces)
    (let ([ns (make-base-empty-namespace)])
        ;(namespace-attach-module (namespace-anchor->empty-namespace a)
        ;    "builtins.rkt"
        ;    ns)
        (parameterize ([current-namespace ns])
            (dynamic-rerequire "builtins.rkt")
            ;(eval '(test-1) ns)
            (let ([n (module->namespace "builtins.rkt")])
            ;(writeln (member 'test-1 (namespace-mapped-symbols (current-namespace))))
            n))))

(define shell-namespace (combine-namespaces))

(define (exec code) 
    (eval code shell-namespace))

(define (reload-shell)
    ;(dynamic-rerequire "builtins.rkt")
    (set! shell-namespace (combine-namespaces))
    )

(define (can-exec)
    (let ([symbols (namespace-mapped-symbols shell-namespace)])
        (writeln (member 'test-1 symbols))
    ))

;(can-exec)

(provide exec reload-shell)
;(define (reload-shell) 
;    (dynamic-rerequire "builtins.rkt"))
    ;(define-values/invoke-unit/infer builtins@)
    ;(let ([ns (make-base-empty-namespace)])
    ;    (namespace-attach-module (namespace-anchor->empty-namespace a)
    ;        "builtins.rkt"
    ;        ns
    ;    )
    ;    (parameterize ([current-namespace ns]) 
     ;       (define-values/invoke-unit builtins@ (import) (export builtins^))
    ;        (void)
    ;    )))


;(reload-shell)
;(define-values/invoke-unit/infer builtins@)
;(test-1)
;(provide reload-shell)