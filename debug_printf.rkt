#lang racket/base

(provide debug-printf)

(define debug? #f)

(define (debug-printf . args)
    (when debug?
        (apply printf args))