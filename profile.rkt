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

(require racket/file)

(provide (all-defined-out))

(define user-profile (expand-user-path "~/.lush_profile"))

(define (setup-profile shell-namespace)

    (unless (file-exists? user-profile)
        (let ([profile (open-output-file user-profile)])
            (close-output-port profile))))

(require racket/rerequire)

(define (source filename shell-namespace)
    (if (file-exists? filename)   
        (parameterize ([current-namespace shell-namespace]) 
        (dynamic-require-for-syntax filename 0))
        #|(let ([input-file (open-input-file filename)])
            (let ([code (read input-file)])
                (eval code shell-namespace)))|#
        ;(load filename)
        (printf "Can't source ~a, file does not exist~n" filename)))
