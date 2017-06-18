#lang racket/base

;provides functions that interact with the low level terminal

(provide 
 ;makes the given process group the foreground
 set-foreground-process-group)

(require ffi/unsafe)

(define libc (ffi-lib #f))

(define tcsetpgrp (get-ffi-obj "tcsetpgrp" libc (_fun _int _int -> _int)))

(define (set-foreground-process-group terminal-descriptor group)
    (tcsetpgrp terminal-descriptor group))
