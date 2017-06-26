#lang racket/base

(provide debug-printf set-debug!)

(define debug? #f)

(define (set-debug! debug) (set! debug? debug))

(define (debug-printf . args)
    (when debug?
        (apply printf args)))