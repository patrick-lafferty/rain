#lang racket/base

(provide 
    pretty-printer%)

(require racket/match)
(require racket/list)
(require racket/class)
(require "interpreter.rkt")
(require "env.rkt")
(require "terminal.rkt")

#|(define bracket-counter 0)
(define (inc-brackets) (set! bracket-counter (add1 bracket-counter)))
(define (dec-brackets) 
    ;(unless (eq? bracket-counter 0)
        (set! bracket-counter (sub1 bracket-counter)))
|#
(define bracket-colours #(68 100 160))
(define (get-bracket-colour x) (vector-ref bracket-colours (modulo x (vector-length bracket-colours))))

(define (set-colour x) (string->list (format "\e[38;5;~am" x)))

(define constant-colour 82)
(define string-colour 173)
(define special-form-colour 125)
(define identifier-colour 27)
(define unknown-colour 211)
(define invalid-bracket-colour 52)

(define (char-delimiter? c)
    (match c
        [#\( #t]
        [#\) #t]
        [#\[ #t]
        [#\] #t]
        [_ #f]))

(define (char-identifier? c)
    (not
        (or (char-whitespace? c)
            (char-delimiter? c))))

(define (add-to-acc acc thing)
    (if (null? acc)
        thing
        (cons acc thing)))

(define (char-not-quote? c)
    (not (eqv? #\" c)))

(define (clamp-line line)
    (let* ([maximum (min (- (getTerminalWidth) 3) (length line))])
        (take-right line maximum)))

(define (clamp x minimum maximum)
    (max (min x maximum) minimum))
#|

(define (x)
    (let ([x 1])
        x))

(
(
)
(
[
]
)
)
)

|#

;TODO: change background to highlight selected bracket pair
;TODO: consider having up/down keys move rows when in multi-line mode instead of history
(define pretty-printer%
    (class object%
        (super-new)

        (define bracket-counter 0)
        (define cumulative-bracket-counter 0)
        (define used-bracket-colours '())
        (define current-line-bracket-colours '())
        (define indent 0)
        (define closers '())

        (define/public (print-line line show-prompt? current-position)
            (set! closers '())
            (set! bracket-counter 0)
            (set! current-line-bracket-colours '())
            (let* ([capped (clamp-line line)]
                    [lexed (lex capped '())]
                    [flattened (flatten lexed)]
                    [line (list->string flattened)]
                    [indented (string-append (make-string (max 0 (+ (if (> indent 0) 2 0) indent)) #\space) line)])
                (refresh-line indented (+ (if (> indent 0) 2 0) indent current-position) show-prompt?)))

        (define (combine-and-match current previous closers)
            (if (null? current) previous
            (let* ([combined (append current previous)]
                   [length-closers (length closers)]
                   [matched
                        (for/fold ([unmatched '()]) ([i combined] [j closers])
                            (if (are-matching? (list (first i) j))
                                unmatched
                                (cons i unmatched)))])
                    (let ([diff (- (length combined) length-closers)])
                        (if (> length-closers 0)
                            (append matched (drop combined length-closers))
                            combined)))))

        (define/public (new-line)
            (set! used-bracket-colours (append current-line-bracket-colours used-bracket-colours))
            (set! cumulative-bracket-counter (+ bracket-counter cumulative-bracket-counter))

            (cond
                [(> bracket-counter 0)
                    (set! indent (+ indent 2))];(* 2 bracket-counter)))]
                [(< bracket-counter 0)
                   (set! indent (- indent 2))]));(* 2 bracket-counter)))]))

        (define/public (reset)
            (set! bracket-counter 0)
            (set! cumulative-bracket-counter 0)
            (set! used-bracket-colours '())
            (set! indent 0))

        (define (are-matching? pair)
            (match pair
                ['(#\( #\)) #t]
                ['(#\[ #\]) #t]
                ['(#\{ #\}) #t]
                [_ #f]))

        (define (get-matching-colour bracket-to-match)
            (let ([colour
                (if (> bracket-counter 0)
                    (begin 
                        (let ([length-colours (length current-line-bracket-colours)])
                            (findf (match-lambda
                                [(list bracket colour) (are-matching? (list bracket bracket-to-match))])
                                        (drop current-line-bracket-colours 
                                            (clamp (sub1 (- length-colours bracket-counter))
                                                0 length-colours)))))
                    (begin 
                        (let ([length-colours (length used-bracket-colours)])
                            (findf (match-lambda 
                                [(list bracket colour) (are-matching? (list bracket bracket-to-match))])
                                    (drop used-bracket-colours
                                        (clamp 
                                             (- length-colours (+ bracket-counter cumulative-bracket-counter))
                                            0
                                            length-colours))))))])
                (if colour
                    (second colour)
                    invalid-bracket-colour)))
                            
        (define (lex lst acc)
            (if (null? lst)
                acc
                (match (first lst)
                    [(or #\( #\[)
                        (let* ([current-bracket-counter bracket-counter]
                                [colour (get-bracket-colour (+ cumulative-bracket-counter current-bracket-counter))])
                            (set! bracket-counter (add1 bracket-counter))
                            (set! current-line-bracket-colours (cons (list (first lst) colour) current-line-bracket-colours))
                            (let ([acc (add-to-acc acc (set-colour colour))])
                                (lex (rest lst) (add-to-acc acc (first lst)))))]
                    [(or #\) #\])
                        (set! closers (cons (first lst) closers))
                        (let* ([current-bracket-counter bracket-counter]
                                [colour (get-matching-colour (first lst))]
                                [acc (add-to-acc acc (set-colour colour))])
                            (set! bracket-counter (sub1 bracket-counter))
                            (set! current-line-bracket-colours (combine-and-match current-line-bracket-colours '() (list (first lst))))
                            (lex (rest lst) (add-to-acc acc (first lst))))]
                    [(? char-numeric?)
                        (let-values ([(number remaining) (splitf-at lst char-numeric?)])
                            (let ([acc (add-to-acc acc (set-colour constant-colour))])
                                (lex remaining (add-to-acc acc number))))]
                    [(? char-whitespace?)
                        (lex (rest lst) (add-to-acc acc (first lst)))]
                    [#\"
                        (let*-values (
                            [(string remaining) (splitf-at (rest lst) char-not-quote?)]
                                [(remaining add-closing-quote?)
                                    (if (null? remaining) (values remaining #f)
                                        (if (eqv? #\" (first remaining)) (values (rest remaining) #t)
                                            (values remaining #f)))])
                            (let ([acc (add-to-acc acc (set-colour string-colour))])
                                (let ([quoted 
                                    (if add-closing-quote? (list #\" string #\")
                                        (list #\" string))])
                                    (lex remaining (add-to-acc acc quoted)))))]
                    [_ 
                        (let-values ([(identifier remaining) (splitf-at lst char-identifier?)])
                            (let* ([symbol (string->symbol (list->string identifier))]
                                    [colour 
                                (cond
                                    [(is-special-form? symbol)
                                        special-form-colour]
                                    [(lookup symbol (list repl-env profile-env))
                                        identifier-colour]
                                    [else unknown-colour])])
                                (let ([acc (add-to-acc acc (set-colour colour))])
                                    (lex remaining (add-to-acc acc identifier)))))]

            )))
    ))



;TODO: replace with prompt from user profile
(define prompt-character 
    (match (system-type 'os)
        ['unix "λ "]
        ['windows "> "]
        ['macosx "λ "]
    ))

(define (refresh-line line current-position [show-prompt? #t])
    (printf "\e[2K") ;ANSI escape code CSI n K - Erase in Line
    (printf "\e[1G") ;ANSI escape code CSI n G - Cursor Horizontal Absolute
    (when show-prompt?
        (display prompt-character))
    (display line)
    (printf "\e[39;49m")
    (printf "\e[~aG" (+ (if show-prompt? 3 1) current-position))
    (flush-output)
    )