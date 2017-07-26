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

(require "shell.rkt"
    "sh-lang.rkt"
    racket/place
    "repl-place.rkt"
    "editor/syntax-highlighter.rkt"
    "functional/maybe.rkt"
    "editor/autocompletion.rkt"
    "interpreter/env.rkt")

(define pretty-printer (new pretty-printer%))

(define (input-loop channel current-line show-prompt? current-position current-row)
    (send pretty-printer print-line current-line show-prompt? current-position current-row)

    ;(set-current-row! current-row)
    ;(set-current-column! current-position)
    
    ;(send screen set-cursor-position current-row current-position)

    (let ([line (place-channel-get channel)])
        (match line
            ['tab
                (send screen add-widget dropdown)

                (let ([completion-candidate (send pretty-printer complete-if-possible current-position)])
                    (match completion-candidate
                        [(some (list x start-index end-index)) 
                            ;(printf "~n~ncompleting ~v between ~v and ~v~n~n" x start-index end-index)
                            (place-channel-put channel (list 'replace x start-index end-index))]
                        [_ (place-channel-put channel 'continue)]))
                (input-loop channel current-line show-prompt? current-position current-row)]
            ['clear 
                (printf "\e[6n")
                (flush-output)
                (send pretty-printer reset)
                (input-loop channel '() #t 0 current-row)]
            ['newline 
                (send screen remove-widget dropdown)

                (printf "\e[6n")
                (flush-output)
                ;(displayln "newline")
                (send pretty-printer new-line)
                (input-loop channel '() #t 0 (add1 current-row))]
            [(list 'finished line)
                (send screen remove-widget dropdown)
                (displayln "")
                (printf "\e[6n")
                (flush-output)

                (let ([line
                    (if show-prompt?
                        (let ([get-prompt-string (lookup 'get-prompt-string (list repl-env profile-env))])
                            (string-append (get-prompt-string) line))
                        line)])

                    (send screen add-line line))

                (send pretty-printer new-line)
                (send pretty-printer reset)

                (with-handlers
                    ([exn:fail? (lambda (e) (writeln e))])
                    (let* ([code (read (open-input-string line))])    
                        (cond
                            [(list? code) (exec code)]
                            [(symbol? code) (handle-symbol code)]
                            [ else (void)])));(printf "unknown: ~a~n" code)])))

                (input-loop channel '() #t 0 current-row)]

            [(list 'incomplete line)
                (printf "\e[6n")
                (flush-output)
                (send pretty-printer new-line)

                (send screen add-line (list->string line))

                (printf "\n")
                (send pretty-printer print-line line #f current-position (add1 current-row))
                (input-loop channel line #f 0 (add1 current-row))]
            [(list 'update show-prompt? line)
                (printf "\e[6n")
                (flush-output)
                (send pretty-printer print-line line show-prompt? current-position current-row)
                (send pretty-printer highlight-matching-bracket current-position)
                (input-loop channel line show-prompt? current-position current-row)]
            [(list 'update-cursor position)
                (printf "\e[~aG" (+ 3 position))
                ;(set-current-column! ( + 3 position))
                
                ;(send screen set-cursor-position current-row (+ 3 position))
                (send screen set-cursor-position current-row (if show-prompt? (+ 3 position) position))

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

(set-definition-handler! 
    (lambda (id) (add-symbol id)))

(define unused-handle (plumber-add-flush! (current-plumber) (lambda (x) 
    (plumber-flush-handle-remove! x)
    (handle-symbol 'exit))))

(require 
    "terminal.rkt"
    "terminal/screen.rkt"
    "terminal/dropdown.rkt"
    "terminal/escape-sequences.rkt")

(enter-cursor-address-mode)

(define screen (new screen% [width (getTerminalWidth)] [height (getTerminalHeight)]))
(define dropdown (new dropdown%
        [lines 
            (list
                "this is line 1"
                "this is line 2"
                "this is a super long line 3"
                "this is line 4")]))

(main)

;(exit-cursor-address-mode)

(handle-symbol 'exit)