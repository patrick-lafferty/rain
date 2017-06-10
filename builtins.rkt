#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

(require "version.rkt")
    
(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace)); (current-namespace)))
        #t
        #f))

(require "jobs.rkt")

(define (expand path) 
    (let ([exp (pregexp (string-replace path "*" "\\W*"))])
;(for ([p (in-directory "/example")]))
        (let ([expanded (filter (lambda (dir) (regexp-match? exp dir)) (directory-list))])
            (displayln expanded))))
    ;(regexp-match? #px"\\W*.txt" "text.txt")


            

;top-level interactive programs that print directly to stdout
(define (launch2 path . arguments) 
    ;(printf "launch ~a ~a~n" path arguments)
    (let* ([flattened-args (flatten (list path arguments #f))]
           [job (new job% [args flattened-args])])   
        ;(printf "flattened args: ~a ~n" flattened-args)
        (send launcher launch-job job #t)))

;child jobs that redirect stdout to a separate buffer
;TODO: need to close fds 
(define (evaluate2 path . arguments) 
    (define job (new job% [args (flatten (list path arguments #f))]))    
    (format "/dev/fd/~a" (send launcher launch-job job #f)))

(define (launch path)
    (lambda arguments
        (let* ([flattened-args (flatten (list path arguments #f))]
           [job (new job% [args flattened-args])])   
            (send launcher launch-job job #t))))

(define (evaluate path)
    (lambda arguments
        (let ([job (new job% [args (flatten (list path arguments #f))])])    
            (format "/dev/fd/~a" (send launcher launch-job job #f)))))


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
                        (quote body ...)))
                (racket-define (id . params) body ...)) ]
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

(provide is-in-namespace? launch evaluate)
        