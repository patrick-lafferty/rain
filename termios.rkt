#lang racket/base

(require ffi/unsafe)
(require racket/class)
(require racket/list)

(define libc (ffi-lib #f))

(define-cstruct _termios 
    (
        [c_iflag _uint32]
        [c_oflag _uint32]
        [c_cflag _uint32]
        [c_lflag _uint32]
        [c_line _uint8]
        [c_cc (_array/list _uint8 32)]
        [c_ispeed _uint32]
        [c_ospeed _uint32]
    ))

(define tcgetattr (get-ffi-obj "tcgetattr" libc (_fun _int _termios-pointer -> _int)))
(define tcsetattr (get-ffi-obj "tcsetattr" libc (_fun _int _int _termios-pointer -> _int)))

(define termios%
    (class object% 
        (init is-shell)
        (super-new)
        (define is-shell? is-shell)
        
        (define tmodes (make-termios 0 0 0 0 0 (make-list 32 0) 0 0))

        (define/public (save-tmodes terminal)
            (tcgetattr terminal tmodes)
            (when is-shell?
                (set-termios-c_lflag! tmodes 35361)))

        (define/public (restore-tmodes terminal)
            (if is-shell?
                (tcsetattr terminal 0 tmodes)
                (tcsetattr terminal 1 tmodes)))
                
        (define/public (quit terminal)
            (tcsetattr terminal 0 tmodes))))

(provide termios%)