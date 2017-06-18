#lang racket/base

;the module provides an alternate read-line because
;read-line on WSL doesn't seem to work

(provide 
 ffi-read-line)

(require ffi/unsafe)

(define libc (ffi-lib #f))
(define getchar (get-ffi-obj "getchar" libc (_fun -> _int)))

(define (ffi-read-line)
    (define (helper acc)
        (let ([c (getchar)])
            (if (and
                    (> c -1)
                    (not (eq? c 10)))
                (helper (cons c acc))
                acc)))
    (let ([line (helper '())])
        (let ([reversed (reverse (map (lambda (i) (integer->char i)) line))])
            (list->string reversed))))
