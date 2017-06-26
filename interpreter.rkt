#lang racket/base

(provide
    interpret)

(require racket/list)
(require racket/match)
(require "debug_printf.rkt")
(require "env.rkt")

(define interpreter-keywords (list 
    'if
    'or
    'and
    'cond
    'set
    'define
    'lambda
    'quote
    'let
    'let*
    'letrec
    'begin
    ))

(define (escape-local x env)
    (match x
        [(list '!!local-or-string y) y]
        [_ x]))

(define (interpret code env [top-level? #f]) 
    (debug-printf "interpreting code ~v~n" code)
    (match code
        ['() code]
        [(? number? n) n]
        [(? boolean? b) b]
        [(list 'if test-expr then-expr else-expr)
            (debug-printf "if test: ~v then: ~v else: ~v~n" test-expr then-expr else-expr)
            (if (interpret test-expr env)
                (interpret then-expr env)
                (interpret else-expr env))]
        [(list 'or) #f]
        [(list 'and) #t]
        [(list 'and exprs ...)
            (letrec ([shortcircuit-interpret
                (lambda (head tail)
                    (if (interpret head env)
                        (if (null? tail)
                            #t
                            (shortcircuit-interpret (first tail) (rest tail)))
                        #f))])
                (shortcircuit-interpret (first exprs) (rest exprs)))]

        [(list 'or exprs ...)
            (letrec ([shortcircuit-interpret 
                (lambda (head tail)
                    (if (interpret head env)
                        #t
                        (if (null? tail)
                            #f
                            (shortcircuit-interpret (first tail) (rest tail)))))])
                (shortcircuit-interpret (first exprs) (rest exprs)))]

        [(list 'cond tests ...)
            (debug-printf "[interpret] cond tests: ~v~n" tests)
            (letrec ([shortcircuit-interpret
                (lambda (head tail)
                    (match head
                        [(list 'else body)
                            (interpret body env)]
                        [(list 'else body ...)
                            (foldl (lambda (x _) (interpret x env)) #f body)]
                        [(list test body)
                            (if (interpret test env)
                                (interpret body env)
                                (if (null? tail)
                                    (void)
                                    (shortcircuit-interpret (first tail) (rest tail))))]
                        [(list test body ...)
                            (if (interpret test env)
                                (foldl (lambda (x _) (interpret x env)) #f body)
                                (if (null? tail)
                                    (void)
                                    (shortcircuit-interpret (first tail) (rest tail))))]
                        [_ (error (format "cond test expects a list, but the test was ~v~n" head))]))])
                (shortcircuit-interpret (first tests) (rest tests)))]

        [(list 'set! a b)
            (set-in-env! a (interpret b env) env)]
        [(list 'define (list id params) body ...)
            (debug-printf "[interpret] define id: ~v params: ~v body: ~v~n" id params body)
            (when top-level? (hash-set! source-env id code))
            (let ([define-env (make-empty-env env)])
                (hash-set! (first env) id (lambda args 
                    (let ([arguments (make-env params (interpret args define-env) env)])
                        (foldl (lambda (x acc) (interpret x arguments)) #f body)))))]
        [(list 'define (list id) body ...)
            (when top-level? (hash-set! source-env id code))
            (let ([define-env (make-empty-env env)])
                (hash-set! (first env) id (lambda _ 
                        (foldl (lambda (x acc) (interpret x define-env)) #f body))))]
        [(list 'define id expr)
            (when top-level? (hash-set! source-env id code))
            (hash-set! (first env) id (interpret expr env))]
        [(list 'lambda params body ...) 
            (debug-printf "[interpret] lambda params:~v body:~v~n" params body)
            (let ([params 
                    (if (list? params) 
                        (map (lambda (x) (escape-local x env)) params)
                        (escape-local params env))])
                (lambda args
                    (debug-printf "[lambda] args: ~v~n" args)
                    (let ([arguments (make-env params (interpret args env) env)])
                        (foldl (lambda (x acc) (interpret x arguments)) #f body))))]
        [(list 'quote a) 
            (debug-printf "[interpret] quote: ~v~n" a)
            a]
        [(list 'let val-exprs body ...)
            (debug-printf "[interpret] let val-exprs: ~v body: ~v~n" val-exprs body)
            (let* ([ids (map (match-lambda [(list id val) (escape-local id env)]) val-exprs)]
                   [values (map (match-lambda [(list id val) (interpret val env)]) val-exprs)])
                   (let ([arguments (make-env ids values env)])
                    (foldl (lambda (x acc) (interpret x arguments)) #f body)))]
        [(list 'let* val-exprs body ...)
            (debug-printf "[interpret] let* val-exprs: ~v body: ~v~n" val-exprs body)
            (let* ([temp-env (make-empty-env env)]
                   [accumulated-env (foldl (lambda (x acc) (update-env (first x) (interpret (second x) acc) acc)) temp-env val-exprs)])
                (foldl (lambda (x acc) (interpret x accumulated-env)) #f body))]

        [(list 'letrec val-exprs body ...)
            (debug-printf "[interpret] letrec val-exprs: ~v body: ~v~n" val-exprs body)
            (let* ([ids (map (match-lambda [(list id val) id]) val-exprs)]
                  [temp-env (make-env ids (map (lambda (x) 0) ids) env)])
                
                (for ([p val-exprs])
                    (update-env (first p) (interpret (second p) temp-env) temp-env))
                (foldl (lambda (x acc) (interpret x temp-env)) #f body))]

        [(list '!!local-or-string a) 
            (debug-printf "[interpret] !!local-or-string a: ~v~n" a)
            (if (member a interpreter-keywords)
                a
                (let ([local? (lookup a env)])
                    (debug-printf "[interpret] [local-or-string] local: ~v~n" local?)
                    (if local?
                        local?
                        (symbol->string a))))]
        [(list 'begin exprs ...)
            (debug-printf "[interpret] begin exprs: ~v~n" exprs)
            (foldl (lambda (x _) (interpret x env)) #f exprs)]
        
        [(list (list 'lambda params body ...) args ...)
            (debug-printf "[interpret] lambda args: ~v~N" args)
            (let ([fn (interpret (cons 'lambda (cons params body)) env)])
                (apply fn args))]

        [(list (or (? symbol? a) (list '!!local-or-string a)) b ...) 
            (debug-printf "[interpret] (or (? symbol? a) (? list? a)): ~v~nb: ~a~n" a b)
            (if (member a interpreter-keywords)
                (interpret (cons a b) env)
                (let ([proc (interpret a env)]) 
                    (debug-printf "interpreted proc ~v~n" proc)
                    (if proc
                        (if (member proc interpreter-keywords)
                            (interpret (cons proc b) env)
                            (if (list? proc)
                                (let ([params (first proc)]
                                    [body (rest proc)])
                                    (debug-printf "params: ~v~nbody: ~v~n" params body)
                                    (let ([arguments (make-env params (interpret b env) env)])
                                        (debug-printf "arguments: ~v~n" arguments)
                                        (map (lambda (x) (interpret x arguments)) body)))

                                (begin 
                                    (debug-printf "applying proc: ~v with b: ~v~n" proc b)
                                    (let ([args (map (lambda (x) (interpret x env)) b)])
                                        (debug-printf "[interpret][apply] args: ~v~n" args)
                                        (apply proc args)))))
                        (error (format "error: proc ~v is undefined~n" a)))))]
        
        [a (debug-printf "interpret symbol: ~v~n" a)
            (if (symbol? a)
                (lookup a env)
                a)]        
        [_ (printf "shouldn't get here: ~v~n" code)]))