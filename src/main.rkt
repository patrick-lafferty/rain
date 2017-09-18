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

(require racket/match
    racket/class
    racket/list
    racket/string)

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

(define showing-dropdown? #f)

(define (update-dropdown current-position)
    (send screen remove-widget dropdown)
    (let ([completion-candidate (send pretty-printer complete-if-possible current-position)])
        (match completion-candidate
            [(some (list candidates start-index end-index))
                (let* ([lines (map (lambda (l) (list->string l)) candidates)]
                    ;[lines (list "line 0" "line 1" "line 2" "line 3" "line 4" "line 5" "line 6" "line 7" "line 8")])
                )
                    ;(printf "~n~n~n~n~n~nlines:~a ~a~n~n" (length lines) lines)
                    (set! dropdown (new selectable-dropdown%
                        [lines lines]))
                    (send screen add-widget dropdown))]
            
            [_ (void)])))


(define (draw-line line show-prompt? current-position current-row)
    (let ([replacement-completion
        (if showing-dropdown?
            (string->list (send dropdown get-selected-item))
            '())])
        (send pretty-printer print-line line show-prompt? current-position current-row replacement-completion)))

(define (hide-autocomplete-dropdown)
    (send screen remove-widget dropdown)
    (set! showing-dropdown? #f))

(define (input-loop channel current-line show-prompt? current-position current-row)
    (draw-line current-line show-prompt? current-position current-row)
    ;(set-current-row! current-row)
    ;(set-current-column! current-position)
    
    ;(send screen set-cursor-position current-row current-position)

    (let ([line (place-channel-get channel)])
        (match line
            ['f1
                (if showing-dropdown?
                    (hide-autocomplete-dropdown) 
                    (begin
                        (update-dropdown current-position)
                        (set! showing-dropdown? #t)))
            ]
            ['up 
                (if showing-dropdown?
                    (begin 
                        (send dropdown select-up)
                        (send screen add-widget dropdown)
                        (place-channel-put channel 'nope))
                    (place-channel-put channel 'history))
                ]
            ['down
                (if showing-dropdown?
                    (begin 
                        (send dropdown select-down)
                        (send screen add-widget dropdown)
                        
                        (place-channel-put channel 'nope))
                    (place-channel-put channel 'history))
            ]
            ['tab

                (let ([completion-candidate (send pretty-printer complete-if-possible current-position)])
                    (match completion-candidate
                        [(some (list x start-index end-index)) 
                            (let ([replacement 
                                (if showing-dropdown? 
                                    (string->list (string-trim (send dropdown get-selected-item)))
                                    (first x))])

                                (place-channel-put channel (list 'replace replacement start-index end-index)))]
                        [_ (place-channel-put channel 'continue)]))

                (hide-autocomplete-dropdown)

                (input-loop channel current-line show-prompt? current-position current-row)]
            ['clear 
                (printf "\e[6n")
                (flush-output)
                (send pretty-printer reset)
                (input-loop channel '() #t 0 current-row)]
            ['newline 
                (hide-autocomplete-dropdown) 

                (printf "\e[6n")
                (flush-output)
                ;(displayln "newline")
                (send pretty-printer new-line)
                (input-loop channel '() #t 0 (add1 current-row))]
            [(list 'finished line)
                (hide-autocomplete-dropdown) 
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
               
                (draw-line line show-prompt? current-position current-row)
                (send pretty-printer highlight-matching-bracket current-position)

                (when showing-dropdown? (update-dropdown current-position))

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
    )
        (input-loop channel current-line show-prompt? current-position current-row)
    ))

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
    "terminal/widgets/selectable-dropdown.rkt"
    "terminal/escape-sequences.rkt")

(enter-cursor-address-mode)

(define screen (new screen% [width (getTerminalWidth)] [height (getTerminalHeight)]))
(define dropdown (new selectable-dropdown%
        [lines 
            (list
                "this is line 1"
                "this is line 2"
                "this is a super long line 3"
                "this is line 4")]))

(main)

;(exit-cursor-address-mode)

(handle-symbol 'exit)