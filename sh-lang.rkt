#lang racket

;the module alters the readtable to handle {} delimiters differently
;by interpreting the contents as shell commands with a different syntax

(require "filesystem.rkt")

(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace))
        #t
        #f))

(define (escape-args args)
    (let ([escape 
        (lambda (arg) 
            (cond
                [(eqv? '#:redirect-in arg) arg]
                [(eqv? '#:redirect-out arg) arg]
                [(eqv? '#:redirect-err arg) arg]
                [(symbol? arg) 
                    (if (is-in-namespace? arg) ;shell-namespace) 
                        arg
                        (symbol->string arg))]
                [else (format "~a" arg)]))])
        (map escape args)))

;transforms any possible executable call (a ...) into (run "a" ...)
(define (escape-executable lst)
    (let ([code (if (list? lst) (reverse lst) lst)])
        (match code
            [(list 'if a b c)  (list 'if (escape-executable a) (escape-executable b) (escape-executable c))]
            [(list a b ...)
                (if (can-execute a)
                    (flatten (list 'run (symbol->string a) (escape-args b)))
                    lst)]
            [_ (if (can-execute code)
                    (list 'run (symbol->string code))
                    code)])))

#|
takes a list of shell commands, escapes runables,
replaces </^/> with keyword args to run func
|#
(define (group lst)
    (define (r current up-to-now groups)    
            (match current
            [(cons a '()) (cons (escape-executable (cons a up-to-now)) groups)]
            [(cons 'pipe tail) (r tail '() (cons (escape-executable up-to-now) groups))]
            [(cons '< tail) (r tail (cons '#:redirect-in up-to-now) groups)]
            [(cons '> tail) (r tail (cons '#:redirect-out up-to-now) groups)]
            [(cons '^ tail) (r tail (cons '#:redirect-err up-to-now) groups)]
            [(cons a b) (r b (cons a up-to-now) groups)]
        ))
    (let ([reversed (reverse (r lst '() '()))])
        (cons 'pipe reversed)))

;the read function used when { is encountered
(define (sh-read-proc char in src ln col pos)
    (define epip '(#\e #\p #\i #\p))
    (define (transform lst)
        (let ([replaced (map (lambda (c) (if (char=? c #\|) epip c)) lst)])
            (flatten (list #\( (reverse (flatten replaced)) #\)))))    
    (define (collect-until delim acc)
        (let ([c (read-char in)])
            (if (char=? c delim) acc (collect-until delim (cons c acc)))))
    (let* ([lst (collect-until #\} '())]
            [str (list->string (transform lst))])
        
        (let ([datum (read (open-input-string str))])
            (let ([grouped (group (flatten datum))])
                grouped))))
        
(current-readtable (make-readtable (current-readtable) #\{ 'terminating-macro sh-read-proc))
