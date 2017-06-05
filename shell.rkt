#lang racket

;This module defines the namespace to be used for
;the shell's REPL

(require racket/rerequire)
(require "filesystem.rkt")

(define (combine-namespaces)
    (dynamic-rerequire "builtins.rkt")
    (let ([n (module->namespace "builtins.rkt")])
        (parameterize ([current-namespace n])
            (namespace-require "filesystem.rkt"))
        n))

(define shell-namespace (combine-namespaces))

(define (unknown e code) 
    (let ([id (exn:fail:contract:variable-id e)])
        (when (eq? (first code) id)
            (printf "~a is unknown ~n" id)
            (when (can-execute id)
                (displayln "it can be executed")))))

(define (exec code) 
    (with-handlers 
        (
            [exn:fail:contract:variable? (lambda (e) (unknown e code))]
            [exn:fail? (lambda (e) (void))])
        (let ([result (eval code shell-namespace)])
            (cond
                [(void? result) (void)]
                [else (displayln result)]))))

(define (handle-symbol s)
    (match s
        ['reload (reload-shell)]
        ['exit (exit)]
        [ _ (printf "~a is undefined ~n" s)]
    ))

(define (reload-shell)
    (set! shell-namespace (combine-namespaces)))

(provide exec reload-shell handle-symbol)