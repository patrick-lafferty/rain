#|
MIT License
Copyright (c) 2017 Patrick Lafferty
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
#lang racket/base

;the module provides a class that interacts with the unix api for terminal i/o

(provide 
 ;class that handles saving/restoring terminal io states
 termios%)

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
