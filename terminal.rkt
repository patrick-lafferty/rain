#lang racket

;(define (loop)
;    (for ([a 256])
;        (display (format "\x1b[48;5;27mX" ))))

;(loop)

;(define (esc code)
 ;   (printf "\x1b[~a" code))

;(require ffi/unsafe)

;(define libc (ffi-lib #f))
;(define getchar (get-ffi-obj "getchar" libc (_fun -> _int)))
;(define peekchar (get-ffi-obj "peekchar" libc (_fun -> _int)))

;(define (rrline)
;    (define (helper acc)
;        (let ([c (getchar)])
;            (writeln c)
;            (if (and
;                    (> c -1)
;                    (not (eq? c 10))
;                    (not (eq? c 82)))
;                (helper (cons c acc))
;                acc)))
;    (let ([line (helper '())])
;        (let ([reversed (reverse (map (lambda (i) (integer->char i)) line))])
;            (list->string reversed))))

;(esc "9999;9999H")
;(esc "38;5;0m")
;(esc "48;5;0m")
;(esc "6n")
;(esc "3J")
;(writeln (peekchar))
;(esc "38;5;27m")
;(define line (rrline))
;(esc "38;5;27m")
;(printf "Response is ~a" line)

(require ffi/unsafe)

(define libc (ffi-lib #f))

(define tcsetpgrp (get-ffi-obj "tcsetpgrp" libc (_fun _int _int -> _int)))

(define (set-foreground-process-group terminal-descriptor group)
    (tcsetpgrp terminal-descriptor group))

(provide set-foreground-process-group)