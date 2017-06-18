#lang racket/base

(provide debug-printf set-debug!)

(define debug? #t)

(define (set-debug! debug) (set! debug? debug))

(define (debug-printf . args)
    (when debug?
        (apply printf args)))