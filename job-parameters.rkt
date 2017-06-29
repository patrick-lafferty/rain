#lang racket/base

(provide is-in-thread?)

(define is-in-thread? (make-parameter #f))