#lang racket

;the module alters the readtable to handle {} delimiters differently
;by interpreting the contents as shell commands with a different syntax

(require "filesystem.rkt")
(require "debug_printf.rkt")

(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace))
        #t
        #f))

(define (escape-args args)
    (letrec ([escape 
        (lambda (arg) 
            (cond
                [(eqv? 'redirect-in arg) arg]
                [(eqv? 'redirect-out arg) arg]
                [(eqv? 'redirect-err arg) arg]
                [(symbol? arg) 
                    (if (is-in-namespace? arg)  
                        arg
                        (list '!!local-or-string arg))]
                [(list? arg) (map escape arg)]
                [else (format "~a" arg)]))])
        (map escape args)))

;transforms any possible executable call (a ...) into (run "a" ...)
(define (escape-executable lst)
    (let ([code (if (list? lst) (reverse lst) lst)])
        (match code
            [(list 'if a b c)  (list 'if (escape-executable a) (escape-executable b) (escape-executable c))]
            [(list a b ...)
                (if (can-execute a)
                    (cons 'run (cons (symbol->string a) (escape-args b)))
                    lst)]
            [_ (if (can-execute code)
                    (list 'run (symbol->string code))
                    code)])))

#|
takes a list of shell commands, escapes runables,
replaces </^/> with keyword args to run func
|#
(define (extract-redirects args [in ""] [out ""] [err ""]) 
    (if (null? args) 
        (list 'list in out err)
        (let ([top (first args)])
            (match (escape-args top)
                [(list 'redirect-in in) (extract-redirects (rest args) in out err)]
                [(list 'redirect-out out) (extract-redirects (rest args) in out err)]
                [(list 'redirect-err err) (extract-redirects (rest args) in out err)]))))

(define (group lst)
    (define (r current up-to-now groups redirects)    
            (match current
            [(cons a '()) (cons (cons (escape-executable (cons a up-to-now)) groups) redirects)]
            [(cons 'pipe tail) (r tail '() (cons (escape-executable up-to-now) groups) redirects)]
            [(list  '< in) (cons (cons (escape-executable up-to-now) groups) (cons (list 'redirect-in in) redirects))] 
            [(list  '< in tail ...) (r tail up-to-now groups (cons (list 'redirect-in in) redirects))]
            [(list  '> out) (cons (cons (escape-executable up-to-now) groups) (cons (list 'redirect-out out) redirects))]
            [(list  '> out tail ...) (r tail up-to-now groups (cons (list 'redirect-out out) redirects))]
            [(list  '^ err) (cons (cons (escape-executable up-to-now) groups) (cons (list 'redirect-err err) redirects))]
            [(list  '^ err tail ...) (r tail up-to-now groups (cons (list 'redirect-err err) redirects))]
            [(cons a b) (r b (cons a up-to-now) groups redirects)]
        ))
    (let ([result (r lst '() '() '())])
        (let ([redirects (rest result)]
              [groups (cons 'list (reverse (first result)))])
              (debug-printf "groups: ~v~n" groups)
              (list 'pipe groups (extract-redirects redirects))))) 

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
            (let ([grouped (group datum)])  
                grouped))))
        
(current-readtable (make-readtable (current-readtable) #\{ 'terminating-macro sh-read-proc))
