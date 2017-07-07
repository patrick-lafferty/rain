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
(require "editor/syntax-highlighter.rkt")

(define pretty-printer (new pretty-printer%))

(define (input-loop channel current-line show-prompt? current-position current-row)
    (send pretty-printer print-line current-line show-prompt? current-position current-row)
    (let ([line (place-channel-get channel)])
        (match line
            ['clear 
                (printf "\e[6n")
                (flush-output)
                (send pretty-printer reset)
                (input-loop channel '() #t 0 current-row)]
            ['newline 
                (printf "\e[6n")
                (flush-output)
                (displayln "newline")
                (send pretty-printer new-line)
                (input-loop channel '() #t 0 (add1 current-row))]
            [(list 'finished line)
                (printf "\e[6n")
                (flush-output)
                (let* ([code (read (open-input-string line))])    
                    (cond
                        [(list? code) (exec code)]
                        [(symbol? code) (handle-symbol code)]
                        [ else (printf "unknown: ~a~n" code)]))
                (send pretty-printer reset)
                (input-loop channel '() #t 0 current-row)]

            [(list 'incomplete line)
                (printf "\e[6n")
                (flush-output)
                (send pretty-printer new-line)
                (send pretty-printer print-line line #f current-position (add1 current-row))
                (input-loop channel line #f 0 (add1 current-row))]
            [(list 'update show-prompt? line)
                (printf "\e[6n")
                (flush-output)
                (send pretty-printer print-line line show-prompt? current-position current-row)
                (input-loop channel line show-prompt? current-position current-row)]
            [(list 'update-cursor position)
                (printf "\e[~aG" (+ 3 position))
                (send pretty-printer highlight-matching-bracket position)
                (flush-output)
                (input-loop channel current-line show-prompt? position current-row)]
            [(list 'cursor-position row column)
                (input-loop channel current-line show-prompt? current-position row)]
    )))

(define (main)
    (let ([p (create-repl-place)])
        (printf "\e[6n")
        (input-loop p '() #t 0 1)))

(main)