#lang typed/racket/base

(provide 
    none
    some
    Maybe)

(struct none ())
(struct (a) some ([value : a]) #:transparent)

(define-type (Maybe a) (U (some a) none))
