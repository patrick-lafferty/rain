#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

(define (test-1) (displayln "line 1"))
(define (test-2) (displayln "line 2"))
(define (test-3) (displayln "line 3"))
    
(define (is-in-namespace symbol)
    (if (member symbol (namespace-mapped-symbols (current-namespace)))
        (printf "~a is in the namespace ~n" symbol)
        (printf "~a is not in the namespace ~n" symbol)))
        