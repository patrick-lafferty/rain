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

(provide 
    pretty-printer%)

(require 
    racket/list
    racket/class
    racket/match
    (except-in "lexer.rkt" flatten)
    "../terminal.rkt"
    "../interpreter/env.rkt"
    "autocompletion.rkt"
    "../functional/maybe.rkt")

(define (clamp-line line)
    (let* ([maximum (min (- (getTerminalWidth) 3) (length line))])
        (take-right line maximum)))

(define (clamp x minimum maximum)
    (max (min x maximum) minimum))

(struct line (characters row-number aci))

(define (set-cursor-row row)
    (printf "\e[~a;H" row)
    (printf "\e[2K") ;ANSI escape code CSI n K - Erase in Line
    (printf "\e[1G") ;ANSI escape code CSI n G - Cursor Horizontal Absolute
    (flush-output))

(define (get-next-line-number lines)
    (let ([lines (accumulated-lines-lines lines)])
        (if (null? lines) 
            0
            (add1 
                (saved-line-index 
                    (first lines)))))) 

(define (write-line line column show-prompt?)
    (printf "\e[2K")
    (printf "\e[1G")

    (when show-prompt?
        (let ([print-prompt (lookup 'print-prompt (list repl-env profile-env))])
            (print-prompt)))

    (display line)
    (printf "\e[39;49m")
    (printf "\e[~aG" (+ (if show-prompt? 3 1) column))
    (flush-output))

(define (expand-highlight acc characters) 
    (let* ([unhighlight (foldl cons acc (reverse (string->list "\e[0m")))]
            [characters (foldl cons unhighlight characters)]
            [highlighted (foldl cons characters (reverse (string->list "\e[48;5;183m")))])
        highlighted))

(define (should-highlight? 
            line-index 
            character-index
            highlighted) 
    (let ([pair (matching-pair line-index character-index)])
        (or (equal? pair (highlighted-pair-first highlighted))
            (equal? pair (highlighted-pair-second highlighted)))))


(define (complete column start end characters context)
    (if (and (<= column end) (>= column start))
        (if (null? characters)
            (none)
            (let-values ([(word colour-code) (splitf-at-right characters (lambda (x) (not (eqv? x #\m))))])
                (match (reverse word) 
                    [(list #\m x ...)
                        (let ([candidates (autocomplete (rest (reverse word)) context)])
                            (match candidates
                                [(none) (none)]
                                [(some '()) (none)]
                                [(some completion) 
                                    ;(printf "~n~n")
                                    ;(for ([c completion])
                                    ;    (display c))
                                    ;(printf "~n~n")
                                    (some (first completion))]))]
                    [_ (none)])))
        (none)))

(define (expand lexed current-highlighted-pair column)
    (reverse 
        (for/fold ([acc '()]) ([i lexed])
            (match i
                [(highlight-point line-index character-index characters) 
                    (if (should-highlight? line-index character-index current-highlighted-pair)
                        (expand-highlight acc characters)
                        (for/fold ([acc acc]) ([j characters])
                            (cons j acc)))]
                [(autocomplete-point context start end characters) 
                    (let ([completion-candidate (complete column start end characters context)])
                        (match completion-candidate
                            [(some completion)
                                (let ([acc (foldl cons acc (set-colour completion 243))])
                                    (for/fold ([acc acc]) ([j characters])
                                        (cons j acc)))]
                            [(none)
                                (for/fold ([acc acc]) ([j characters])
                                    (cons j acc))]))]
                                
                [ (? list?)
                    (for/fold ([acc acc]) ([j i])
                        (cons j acc))]
                [_ (cons i acc)]

            ))))

(define pretty-printer%
    (class object%
        (super-new)
        (define current-accumulated-lines (make-empty-accumulated-lines))
        (define current-line #f)
        (define indent 0)
        (define show-prompt? #t)
        (define cached-characters '())
        (define highlighted (make-empty-highlighted-pair))
        (define current-row -1)

        (define (do-print acc indent column show-prompt?)
                (let* ([expanded (expand acc highlighted column)]
                        [flattened (flatten expanded)]
                        [string (list->string (reverse flattened))]
                        [column (max column 0)]
                        [indented (string-append (make-string (max 0 (- indent column)) #\space) string)])
                    (write-line indented indent show-prompt?)))

        (define/public (print-line characters show-promptt? column row)
            (when show-prompt? (set! current-row row))

            (let-values ([(acc line lines)
                    (lex 
                        (clamp-line characters) 
                        '() 
                        0
                        (make-empty-saved-line (get-next-line-number current-accumulated-lines))
                        current-accumulated-lines
                        highlighted)])
                (do-print acc (+ (if (> indent 0) 2 0) indent column) column show-prompt?)
                ;(set! current-line line)
                (let ([line 
                    (struct-copy saved-line line
                        [lexed acc]
                        [indent (+ (if (> indent 0) 2 0) indent )])])
                    (set! current-line line))
                (set! cached-characters characters)))

        (define/public (new-line)
            ;lex one last time to get the proper matches so there aren't
            ;any orphans in the previous lines
            (let-values ([(acc line lines)
                    (lex 
                        (clamp-line cached-characters) 
                        '() 
                        0
                        (make-empty-saved-line (get-next-line-number current-accumulated-lines))
                        current-accumulated-lines
                        highlighted)])
                (let ([line 
                    (struct-copy saved-line line
                        [lexed acc]
                        [indent (+ (if (> indent 0) 2 0) indent )])])
                    (set! current-line line)
                    (set! current-accumulated-lines (add-line-to-accumulated line lines))))
                    
            (set! show-prompt? #f)
            (do-print (saved-line-lexed current-line) (saved-line-indent current-line) -1 show-prompt?)
            (remove-old-highlight)
            (let ([bracket-counter (saved-line-bracket-counter current-line)])
                (cond
                    [(> bracket-counter 0)
                        (set! indent (+ indent 2))]
                    [(< bracket-counter 0)
                        (set! indent (- indent 2))])))

        (define/public (reset)
            (remove-old-highlight)
            (printf "\e[~a;H" (+ current-row 1 (saved-line-index current-line)))
            (set! current-accumulated-lines (make-empty-accumulated-lines))
            (set! current-line #f)
            (set! indent 0)
            (set! show-prompt? #t)
            (set! cached-characters '())
            (set! highlighted (make-empty-highlighted-pair))
            (set! current-row -1)
        )

        (define (highlight line-index)
            (let ([line (findf 
                            (lambda (line) (eqv? (saved-line-index line) line-index))
                            (accumulated-lines-lines current-accumulated-lines))])
                (if line
                    (begin
                        (printf "\e[~a;H" (+ current-row 0 line-index))
                        (do-print (saved-line-lexed line) (saved-line-indent line) 0 (eqv? line-index 0)))

                    (when (equal? line-index (saved-line-index current-line))
                        (printf "\e[~a;H" (+ current-row 0 line-index))
                        (do-print (saved-line-lexed current-line) (saved-line-indent current-line) 0 (eqv? line-index 0))))))

        (define (highlight-pair key line matched-pair)
            (if (integer? key)
                (begin
                    (set! highlighted (highlighted-pair (matching-pair line key) matched-pair))
                    (let ([current-line-index line] 
                           [first-line-index (matching-pair-line (highlighted-pair-first highlighted))]
                           [second-line-index (matching-pair-line (highlighted-pair-second highlighted))])
                            (highlight second-line-index)
                            (printf "\e[~a;H" (+ current-row current-line-index))
                            ))
                ;TODO: add foreign-key support here for when multi-line editing is added, so a cursor on line x
                ;can highlight a matching bracket on x + n instead of just x - n
                (void)
            ))

        (define (remove-old-highlight)
            (let* ([copy (struct-copy highlighted-pair highlighted)]
                    [first (highlighted-pair-first copy)]
                    [second (highlighted-pair-second copy)])
                (set! highlighted (make-empty-highlighted-pair))
                (highlight (matching-pair-line first))
                (unless (eqv? (matching-pair-line first) (matching-pair-line second))
                    (highlight (matching-pair-line second)))
                (printf "\e[~a;H" (+ current-row (saved-line-index current-line)))))

        (define (try-highlight-at matching-pairs offset)
            (if (hash-has-key? matching-pairs offset)
                (begin
                    (highlight-pair offset (saved-line-index current-line) (hash-ref matching-pairs offset))
                    #t)
                (if (hash-has-key? matching-pairs (foreign-key offset))
                    ;TODO: same as todo in highlight-pair
                    #f;(void)
                    #f;(void))))
                )))

        (define/public (highlight-matching-bracket column)
            (unless (is-highlighted-empty? highlighted)
                (remove-old-highlight))

            (when current-line
                (let ([matching-pairs (saved-line-matching-pairs current-line)]
                        [offset column])
                    (unless (try-highlight-at matching-pairs offset)
                        (try-highlight-at matching-pairs (sub1 offset))))))

        (define/public (complete-if-possible column)
            ;(printf "~n~n~n~v~n~n" current-line)
            (if current-line
                (let ([completion-point (findf (match-lambda
                        [(autocomplete-point context start end characters)
                            (and (<= column end) (>= column start))]
                        [_ #f]) (saved-line-lexed current-line))])
                    (if completion-point
                        (let ([start-index (autocomplete-point-start-index completion-point)]
                                [end-index (autocomplete-point-end-index completion-point)]
                                [context (autocomplete-point-context completion-point)])
                            (let ([candidate 
                                    (complete column
                                        start-index
                                        end-index
                                        (autocomplete-point-characters completion-point)
                                        context)])
                                (match candidate
                                    [(some x) (some (list x start-index end-index))]
                                    [_ (none)])))
                        (none)))
                (none)))

        #|(define/public (expand column)
            (unless (highlight-matching-bracket column)
                (let ([autocomplete (findf (match-lambda
                        [(autocomplete start end characters)
                            (and (<= column end) (>= column start))
                        [_ #f]) (saved-line-lexed current-line))])
                    (when autocomplete
|#

    ))