#lang racket/base

(require rackunit
    rackunit/text-ui
    racket/list
    "../interpreter.rkt"
    "../env.rkt"
    "../debug_printf.rkt")

(define-test-suite if-tests
    (test-case 
        "test-expr gets evaluated"
        (let ([env (list (make-hash))])
            (test-begin
                (let* ([code '(if (> 2 1) 'yes 'no)]
                        [result (interpret code env)])
                    (check-equal? result 'yes))
                (let* ([code '(if (< 2 1) 'yes 'no)]
                        [result (interpret code env)])
                    (check-equal? result 'no)))))

    (test-case
        "if short-circuits"
        (test-begin
            (let ([env (list (make-hash))])
                (hash-set! (first env) 'x 0)
                (hash-set! (first env) 'y 0)
                (let* ([code '(if (> 2 1) (begin (set! x 1) 'yes) (begin (set! y 1) 'no))]
                        [result (interpret code env)])
                    (check-equal? result 'yes)
                    (check-equal? (lookup 'x env) 1 "x is not 1")
                    (check-equal? (lookup 'y env) 0 "y is not 0")))
            (let ([env (list (make-hash))])
                (hash-set! (first env) 'x 0)
                (hash-set! (first env) 'y 0)
                (let* ([code '(if (< 2 1) (begin (set! x 1) 'yes) (begin (set! y 1) 'no))]
                        [result (interpret code env)])
                    (check-equal? result 'no)
                    (check-equal? (lookup 'x env) 0 "x is not 0")
                    (check-equal? (lookup 'y env) 1 "y is not 1")))))
    )

(define-test-suite set-tests
    (test-case 
        "set! sets a constant"
        (let ([env (make-env 'x 0 '())]
            [code '(set! x 1)])  
            (interpret code env)
            (check-equal? (lookup 'x env) 1 "x is not 1")))
    (test-case
        "set! evaluates expr and then assigns to id"
        (let ([env (make-env 'x 0 '())]
               [code '(set! x (add1 x))])
            (interpret code env)
            (check-equal? (lookup 'x env) 1 "x is not 1")))
)

(define-test-suite begin-tests 
    (let ([env (make-env 'l '() '())])
        (test-case 
            "all exprs get evaluated in order left to right"
            (let* ([code '(begin 
                    (set! l (cons 1 l))
                    (set! l (cons 2 l))
                    (set! l (cons 3 l)))]
                   [result (interpret code env)])
                (check-equal? (lookup 'l env) '(3 2 1) "l is not '(3 2 1)")))
        (test-case 
            "result is the result of the last expr"
            (let* ([code '(begin (+ 1 2) (+ 2 3))]
                    [result (interpret code env)])
                (check-equal? result 5 "result is not 5")))
))

(define-test-suite proc-apply-tests
    (test-case
        "can apply a lambda"
        (let* ([code '((lambda (x y) (+ x y)) 1 2)]
                [result (interpret code (list (make-hash)))])
            (check-equal? result 3 "result is not 3")))

    (test-case
        "can apply a defined proc"
        (let* ([env (make-empty-env '())]
                [definition '(define (f x) (+ x 2))]
                [code '(f 1)])
            (interpret definition env)
            (let ([result (interpret code env)])
                (check-equal? result 3 "result is not 3"))))
)

(define-test-suite let-tests
    (test-case
        "can bind an expr to an id"
        (let* ([env (make-empty-env '())]
                [code '(let ([x ((lambda (x) (add1 x)) 1)]) x)]
                [result (interpret code env)])
            (check-equal? result 2 "result is not 2 ")))
)

(define-test-suite cond-tests
    (test-case 
        "returns the result of the true condition"
        (let* ([env (list (make-hash))]
                [code '(cond 
                    [#t 1]
                    [#f 2])]
                [result (interpret code env)])
            (check-equal? result 1 "result is not 1")))

    (test-case 
        "looks for first true test, ignores rest"
        (let* ([env (make-env 'x -1 '())]
                [code '(cond
                    [#f (set! x 0) #f]
                    [#t (set! x 1) #t]
                    [#t (set! x 2) #f])]
                [result (interpret code env)])
            (check-equal? (lookup 'x env) 1)))

    (test-case 
        "can handle exprs for test"
        (let* ([env (list (make-hash))]
                [code '(cond
                    [(symbol? "a") 'symbol]
                    [(string? "b") 'string])]
                [result (interpret code env)])
            (check-equal? result 'string "result is not 'string")))

    (test-case
        "can handle else"
        (let* ([env (list (make-hash))]
                [code '(cond
                    [#f 'no]
                    [else 'no 'yes])]
                [result (interpret code env)])
            (check-equal? result 'yes "result is not 'yes")))
)

(define-test-suite full-module-tests
    (test-case 
        "filesystem.rkt"
        (let* ([env (list (make-hash))]
                [source 
                    '(define (can-execute path)
                        (let ([filename 
                                (cond 
                                    [(symbol? path) (symbol->string path)]
                                    [(string? path) path]
                                )])
                            (or (find-executable-path filename)
                                (find-executable-path (format "~a.exe" filename)))))]
                [code '(can-execute "ls")]
                [result (interpret source env)]
                [result (interpret code env)])
            (check-equal? result #t "result is #f")))
              
            
            
)

;(run-tests if-tests)
;(run-tests set-tests)
;(run-tests begin-tests)
;(run-tests proc-apply-tests)
;(run-tests let-tests)
;(set-debug! #t)
(run-tests cond-tests)
;(run-tests full-module-tests)