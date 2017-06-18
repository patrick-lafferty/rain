#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

(require "version.rkt")
    
(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace))
        #t
        #f))


(define (expand path) 
    (let ([exp (pregexp (string-replace path "*" "\\W*"))])
;(for ([p (in-directory "/example")]))
        (let ([expanded (filter (lambda (dir) (regexp-match? exp dir)) (directory-list))])
            (displayln expanded))))
    ;(regexp-match? #px"\\W*.txt" "text.txt")
            




(provide is-in-namespace?)
        