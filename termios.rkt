#lang racket

(require ffi/unsafe)

(define libc (ffi-lib #f))

(define-cstruct _termios 
    (
        [c_iflag _uint64]
        [c_oflag _uint64]
        [c_cflag _uint64]
        [c_lflag _uint64]
        [c_cc (_list i _byte)]
        [c_ispeed _int64]
        [c_ospeed _int64]
    ))

(define tcgetattr (get-ffi-obj "tcgetattr" libc (_fun _int _termios-pointer -> _int)))
(define tcsetattr (get-ffi-obj "tcsetattr" libc (_fun _int _int _termios-pointer -> _int)))

(define termios%
    (class object% 
        (super-new)
        (define tmodes (make-termios 0 0 0 0 (make-list 20 0) 0 0))

        (define/public (save-tmodes terminal)
            (tcgetattr terminal tmodes))

        (define/public (restore-tmodes terminal)
            (tcsetattr terminal 1 tmodes))))

(provide termios%)