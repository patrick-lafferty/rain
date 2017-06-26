#lang racket/base

(provide
    set-shell-namespace!
    lookup
    set-in-env!
    make-env
    make-empty-env
    update-env  
    profile-env
    repl-env
    source-env
    reset-repl-env
    export-to-file
    export-to-profile
    print-source  
)

(require
    racket/list
    "profile.rkt"
    "debug_printf.rkt")

(define shell-namespace #f)

(define (set-shell-namespace! ns) (set! shell-namespace ns))

(define (lookup x env) 
    (debug-printf "lookup x: ~v env: ~v~n" x env)
    (cond
        [(null? env) 
            (namespace-variable-value x #t (lambda _ #f) shell-namespace)]
        [else 
            (if (symbol? x)
                (let ([current (first env)])
                    (if (hash-has-key? current x)
                        (hash-ref current x)
                        (lookup x (rest env))))
                x)]))

(define (set-in-env! x value env)
    (debug-printf "set-in-env! x: ~v value: ~v env: ~v~n" x value env)
    (if (null? env)
        (namespace-set-variable-value! x value #t shell-namespace)
        (when (symbol? x)
            (let ([current (first env)])
                (if (hash-has-key? current x)
                    (hash-set! current x value)
                    (set-in-env! x value (rest env))))))) 
        

(define (make-env params args parent)
    (debug-printf "make-env params: ~v args: ~v~n" params args)
    (if (list? params)
        (let ([pairs (map cons params args)])
            (cons (make-hash pairs) parent))
        (cons (make-hash (list (list params args))) parent)))

(define (update-env params args parent)
    (if (list? params)
        (begin 
            (for ([p (map cons params args)])
                (set-in-env! (car p) (cdr p) parent))
            parent)
        (begin 
            (set-in-env! params args parent)
            parent)))

(define (make-empty-env parent) (cons (make-hash) parent))

(define profile-env (make-hash))
(define repl-env (make-hash))
(define source-env (make-hash))

(define (reset-repl-env) 
    (hash-clear! repl-env)
    (hash-clear! source-env))

(define (export-to-file id filename) 
    (let ([value (lookup id (list source-env))])
        (let ([output (open-output-file filename #:exists 'append)])
            (writeln value output) 
            (close-output-port output))
        (printf "Can't export ~a, it's not defined" id)))

(define (export-to-profile id)
    (export-to-file id user-profile))

(define (print-source id)
    (let ([value (lookup id (list source-env))])
        (when value (displayln value))))