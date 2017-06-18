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
                [(eqv? 'redirect-in arg) arg]
                [(eqv? 'redirect-out arg) arg]
                [(eqv? 'redirect-err arg) arg]
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
(define (extract-redirects args [in ""] [out ""] [err ""]) ;#:redirect-in [in ""] #:redirect-out [out ""] #:redirect-err [err ""])
    (if (null? args) 
        (list in out err)
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
            [(list  '< in tail ...) (r tail up-to-now groups (cons (list 'redirect-in in) redirects))]
            [(list  '> out) (cons (cons (escape-executable up-to-now) groups) (cons (list 'redirect-out out) redirects))]
            [(list  '> out tail ...) (r tail up-to-now groups (cons (list 'redirect-out out) redirects))]
            [(list  '^ err tail ...) (r tail up-to-now groups (cons (list 'redirect-err err) redirects))]
            [(cons a b) (r b (cons a up-to-now) groups redirects)]
        ))
    (let ([result #|(reverse|# (r lst '() '() '())])
        (let ([redirects (rest result)]
              [groups (cons 'list (reverse (first result)))])
              (printf "groups: ~v~n" groups)
              (list 'pipe groups (extract-redirects redirects))))) ;(keyword-apply extract-redirects rs fs))))))
        ;(cons 'pipe reversed)))

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
