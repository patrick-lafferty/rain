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
#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

(require "version.rkt")
    
(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace))
        #t
        #f))


(define (expand path) 
    (let ([exp (pregexp (string-replace path "*" "\\W*"))])
;(for ([p (in-directory "/example")]))
        (let ([expanded (filter (lambda (dir) (regexp-match? exp dir)) (directory-list))])
            (displayln expanded))))
    ;(regexp-match? #px"\\W*.txt" "text.txt")
            




(provide is-in-namespace?)
        