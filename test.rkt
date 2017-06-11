#lang racket

(require "sh-lang/lang/reader.rkt")
(require "filesystem.rkt")

(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace))
        #t
        #f))

(define (escape-args args)
    (let ([escape 
        (lambda (arg) 
            (cond
                [(eqv? '#:redirect-out arg) arg]
                [(eqv? '#:redirect-in arg) arg]
                [(symbol? arg) 
                    (if (is-in-namespace? arg) 
                        arg
                        (symbol->string arg))]
                [else (format "~a" arg)]))])
        (map escape args)))

(define (escape-executable lst)
    (let ([code (if (list? lst) (reverse lst) lst)])
        (match code
            [(list 'if a b c)  (list 'if (escape-executable a) (escape-executable b) (escape-executable c))]
            [(list a b ...)
                (if (can-execute a)
                    (flatten (list 'run a (escape-args b)))
                    lst)]
            [_ (if (can-execute code)
                    (list 'run code)
                    code)])))

(define (group lst)
    ;takes a list and splits by ['pipe, 'redirect-out])

    (define (r current up-to-now groups)    
            (match current
            [(cons a '()) (cons (escape-executable (cons a up-to-now)) groups)]
            [(cons 'pipe tail) (r tail '() (cons (escape-executable up-to-now) groups))]
            [(cons '> tail) (r tail (cons '#:redirect-out up-to-now) groups)]
            [(cons '< tail) (r tail (cons '#:redirect-in up-to-now) groups)]
            [(cons a b) (r b (cons a up-to-now) groups)]
        ))

    (cons 'pipe (reverse (r lst '() '()))))

(define str "{ls *.txt | (if c sort unsort) | less > out.txt}")

(current-readtable (make-readtable (current-readtable) #\{ 'terminating-macro sh-read-proc))

(define code (read  (open-input-string str)))
(define grouped-code (group (flatten code)))

(writeln grouped-code)