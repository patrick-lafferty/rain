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

(require racket/match)
(require racket/class)
(require racket/list)

;Basic shell that uses Racket as its scripting language

(require "shell.rkt")
(require "sh-lang.rkt")
(require racket/place)
(require "repl-place.rkt")
(require "syntax-colourizer.rkt")

(define (input-loop channel current-line show-prompt?)
    #|(with-handlers
        ([exn:fail? (lambda (e) (displayln e))]
         [exn:fail:contract? (lambda (e) (displayln e))])|#
    (print-colour-syntax current-line show-prompt?)
    (let ([line (place-channel-get channel)])
        ;(printf "got ~v~n" line)
        (match line
            [(list 'finished line)
                ;(print-colour-syntax (string->list line) #f)
                (let* (;[line (list->string line)]
                        [code (read (open-input-string line))])    
                    ;(printf "interpreting ~v~n" line)
                    (cond
                        [(list? code) (exec code)]
                        [(symbol? code) (handle-symbol code)]
                        [ else (printf "unknown: ~a~n" code)]))
                (input-loop channel '() #t)]

            [(list 'incomplete line)
                (print-colour-syntax line #f)
                (input-loop channel line #f)]
            [(list 'update show-prompt? line)
                (print-colour-syntax line show-prompt?)
                (input-loop channel line show-prompt?)]))
    )
    ;(input-loop channel current-line))

(define (main)
    (let ([p (create-repl-place)])
        (input-loop p '() #t)))

(main)