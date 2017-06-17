#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

(require "version.rkt")
    
(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace))
        #t
        #f))

;(require "jobs.rkt")

(define (expand path) 
    (let ([exp (pregexp (string-replace path "*" "\\W*"))])
;(for ([p (in-directory "/example")]))
        (let ([expanded (filter (lambda (dir) (regexp-match? exp dir)) (directory-list))])
            (displayln expanded))))
    ;(regexp-match? #px"\\W*.txt" "text.txt")
            

(define (run name [args '()] #:redirect-in [in ""] #:redirect-out [out ""] #:redirect-err [err ""])
    (list (list name args) in out err))

#|(define (pipe . jobs)
    (let ([jjobs (map (lambda (j) 
        (new job% [args (flatten (list (first j) #f))]
                [redirects (rest j)]
            )) jobs)])
        (send launcher launch-group jjobs)))
|#
(require (only-in racket/base (define racket-define)))

(define sexp-table (make-hash))

(define-syntax define
    (syntax-rules ()
        [(_ (id . params) body ...) 
            (begin
                (hash-set! sexp-table (quote id) 
                    (list 
                        'define
                        (flatten 
                            (list 
                                (quote id) 
                                (quote params))) 
                        (quote (flatten (map (lambda (b) (quote b)) body ...))))) 
                (racket-define (id . params) body ...))]
        [(_ other ...) (racket-define other ...)]
                     
    ))

(require "profile.rkt")

(define (export-to-file sexp-id filename) 
    (if (hash-has-key? sexp-table sexp-id)
        (let ([output (open-output-file filename #:exists 'append)])
            (write (hash-ref sexp-table sexp-id) output)
            (close-output-port output))
        (printf "Can't export ~a, it's not defined" sexp-id)))

(define (export-to-profile sexp-id)
    (export-to-file sexp-id user-profile))

(provide is-in-namespace?)
        