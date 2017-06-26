#lang racket/base

(require rackunit
    rackunit/text-ui
    racket/list
    "../interpreter.rkt"
    "../env.rkt")

(define if-tests
    (test-suite
        "if form tests for the interpreter"
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
                        


    ))

        
(run-tests if-tests)