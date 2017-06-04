#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

;(require "sig.rkt")

;(define-unit builtins@
;    (import)
;    (export builtins^)
;    
;    (displayln "loaded builtins")

(define (test-1) (displayln "line 1"))
(define (test-2) (displayln "line 2"))
(define (test-3) (displayln "line 3"))
    
;    )
(provide test-1)
;(provide builtins@)

;(provide test test2)

;(define (test)
;    (display "test"))

;(define (test2)
;    (displayln "reloaded"))