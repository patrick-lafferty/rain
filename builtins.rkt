#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

(define (test-1) (displayln "line 1"))
(define (test-2) (displayln "line 2"))
(define (test-3) (displayln "line 3"))
    
(define (is-in-namespace? symbol)
    (if (member symbol (namespace-mapped-symbols (current-namespace)))
        #t
        #f))

(require "jobs.rkt")

;top-level interactive programs that print directly to stdout
(define (launch path . arguments) 
    ;(printf "launch ~a ~a~n" path arguments)
    (let* ([flattened-args (flatten (list path arguments #f))]
           [job (new job% [args flattened-args])])   
        ;(printf "flattened args: ~a ~n" flattened-args)
        (send launcher launch-job job #t)))

;child jobs that redirect stdout to a separate buffer
;TODO: need to close fds 
(define (evaluate path . arguments) 
    (define job (new job% [args (flatten (list path arguments #f))]))    
    (format "/dev/fd/~a" (send launcher launch-job job #f)))

(provide is-in-namespace?)
        