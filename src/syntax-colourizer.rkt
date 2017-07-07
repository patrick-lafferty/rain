#lang racket/base

(provide 
    pretty-printer%)

(require racket/match)
(require racket/list)
(require racket/class)
(require racket/hash)
(require "interpreter.rkt")
(require "env.rkt")
(require "terminal.rkt")

(define bracket-colours #(68 100 160))
(define (get-bracket-colour x) (vector-ref bracket-colours (modulo x (vector-length bracket-colours))))

(define (set-colour x) (string->list (format "\e[38;5;~am" x)))
(define highlight (string->list "\e[48;5;183m"));"\e[7m"))
(define normal (string->list "\e[0m"))

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

(struct line (characters row-number aci))

(define (set-cursor-row row)
    ;(printf "~nset-cursor-row: ~a~n" row)
    (printf "\e[~a;H" row)
    (printf "\e[2K") ;ANSI escape code CSI n K - Erase in Line
    (printf "\e[1G") ;ANSI escape code CSI n G - Cursor Horizontal Absolute
    (flush-output))

#|
the problem is the current line kees getting regenerated, and the past lines are collapsed into one
and too much mutable state

need to make things local

(struct line (
    characters ;list of printable characters to be lexed again
    matching-pairs ;hash-set of character index to (line-index, character-index)
    bracket-counter ; sum of #-of-openers and #-of-closers
    used-colours ;hash-set of character index to colour 
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
        (define line-index 0)
        (define current-row-number 0)
        (define lines (make-hash))
        (define current-characters '())
        (define current-character-index 0)
        (define highlighted-indices '(-1 -1))

        (define/public (print-line line show-prompt? current-position current-row [highlight-index -1])
            (hash-clear! current-closing-pairs)
            (set! closers '())
            (set! bracket-counter 0)
            (set! current-line-bracket-colours '())
            (set! current-row-number current-row)
            (set! current-character-index 0)
            (let* ([capped (clamp-line line)]
                    [lexed (lex capped '() accumulated-character-index highlight-index)]
                    [flattened (flatten lexed)]
                    [line (list->string flattened)]
                    [indented (string-append (make-string (max 0 (+ (if (> indent 0) 2 0) indent)) #\space) line)])
                (set! current-characters capped)
                (refresh-line indented (+ (if (> indent 0) 2 0) indent current-position) current-row show-prompt?)))

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
            (hash-union! closing-pairs current-closing-pairs)
            (hash-clear! current-closing-pairs)
            (clear-highlights)
            (set! used-bracket-colours (append current-line-bracket-colours used-bracket-colours))
            (set! cumulative-bracket-counter (+ bracket-counter cumulative-bracket-counter))
            (hash-set! lines current-row-number #|line-index|# 
                (line current-characters current-row-number accumulated-character-index))
            (set! current-characters '())
            (set! line-index (add1 line-index))
            (set! accumulated-character-index (+ accumulated-character-index current-character-index))
            (cond
                [(> bracket-counter 0)
                    (set! indent (+ indent 2))]
                [(< bracket-counter 0)
                   (set! indent (- indent 2))]))

        (define/public (reset)
            (set! bracket-counter 0)
            (set! cumulative-bracket-counter 0)
            (set! used-bracket-colours '())
            (set! current-line-bracket-colours '())
            (set! indent 0)
            (set! closers '())
            (set! line-index 0)
            (set! current-row-number 0)
            (hash-clear! lines)
            (set! current-characters '())
            (set! current-character-index 0)
            (set! highlighted-indices '(-1 -1))
            (set! accumulated-character-index 0)
            (hash-clear! current-closing-pairs)
            (hash-clear! closing-pairs)
            (hash-clear! characters)
        ) 

        (define (are-matching? pair)
            (match pair
                ['(#\( #\)) #t]
                ['(#\[ #\]) #t]
                ['(#\{ #\}) #t]
                [_ #f]))

#|
bracket highlighting:

each bracket additionally stores character id, line-character-id, line id     ;, (row, col local to multiline not shell, always starts at 0)

line list:
buffered line chars before list->string was called

closing pairs list:

open-character-id close-character-id

-lookup char under cursor
-if bracket
  -lookup pair
  insert \e[background highlight] to line, redraw line

redrawing lines:
take actual row number at start, add line id to get actual row of line

need to get screen height to see if it scrolled
if start-row + line-count > screen-height
then last line is max row, start-row is screen-height - (start-row + linecount - screen-height)

vim bindings?
|#      
        (define accumulated-character-index 0)
        (struct character (line-index line-character-index glyph))
        (define current-closing-pairs (make-hash))
        (define closing-pairs (make-hash))
        (define characters (make-hash))

        (define (clear-highlights)
            ;(printf "[ch] hl: ~v cp: ~v~n" highlighted-indices closing-pairs)
            (unless (equal? '(-1 -1) highlighted-indices)
                (let* ([opener (hash-ref characters (first highlighted-indices))]
                        [closer (hash-ref characters (second highlighted-indices))]
                        [line-opener (safe-get-line opener current-row-number)]
                        [line-closer (safe-get-line closer current-row-number)])
                        (void)
                    ;(printf "\e[~a;H" (line-row-number line-opener))
                    #|(set! highlighted-indices '(-1 -1))
                    (printf "~nclearing opener ~v~n" (line-row-number line-opener))
                    (set-cursor-row (line-row-number line-opener))
                    (print-line (line-characters line-opener) #f 0 (line-row-number line-opener) 0)
                    ;(printf "\e[~a;H" (line-row-number line-closer))
                    (printf "~nclearing closer ~v~n" (line-row-number line-closer))
                    (set-cursor-row (line-row-number line-closer))
                    (print-line (line-characters line-closer) #f 0 (line-row-number line-closer) 0)
                    (set-cursor-row (add1 current-row-number))
                    |#
                    )))

        (define (is-opener? glyph)
            (match glyph
                [#\( #t]
                [#\[ #t]
                [_ #f]))

        (define (safe-get-line character current-row-number)
            (if (hash-has-key? lines (character-line-index character))
                (hash-ref lines (character-line-index character))
                (line current-characters current-row-number accumulated-character-index)))

        (define (highlight-pair character-index matching-bracket-index)
            ;(printf "lines: ~v~n" lines)
            (let* ([first-character (hash-ref characters character-index)]
                    [second-character (hash-ref characters matching-bracket-index)]
                    [opener (if (is-opener? (character-glyph first-character)) first-character second-character)]
                    [closer (if (is-opener? (character-glyph first-character)) second-character first-character)]
                    [line-opener (safe-get-line opener current-row-number)]
                    [line-closer (safe-get-line closer current-row-number)])
                (set! highlighted-indices
                    (if (is-opener? (character-glyph first-character))
                        (list character-index matching-bracket-index)
                        (list matching-bracket-index character-index)))
                (let ([current-row-backup current-row-number]
                        [aci accumulated-character-index])
                    (set-cursor-row (line-row-number line-opener))
                    (set! accumulated-character-index (line-aci line-opener))
                    (print-line (line-characters line-opener) #f 0 (line-row-number line-opener) 0)
                    (set-cursor-row (line-row-number line-closer))
                    (set! accumulated-character-index (line-aci line-closer))
                    (print-line (line-characters line-closer) #f 0 (line-row-number line-closer) 0)
                    (set! current-row-number current-row-backup)
                    (set! accumulated-character-index aci)
                    (set-cursor-row (add1 current-row-number)))
        
            ))




            ;(let* ([character (hash-ref characters matching-bracket-index)]
            ;        [line (safe-get-line character)])
                ;(printf "\e[~a;H" (line-row-number line))
                #|(set-cursor-row (line-row-number line))
                (set! highlighted-indices 
                    (if (is-opener? (character-glyph character)) 
                        (list matching-bracket-index character-index)
                        (list character-index matching-bracket-index)))
                (print-line (line-characters line) #f 0 (line-row-number line) matching-bracket-index)
                |#;(void)))

        (define/public (highlight-matching-bracket [current-position -1])
            (let* ([current-position 
                        (if (> current-position -1)
                            current-position
                            current-character-index)]
                    [character-index (+ accumulated-character-index current-position)])
                ;(printf "~ncp: ~v ci: ~v clp: ~v~n cs: ~v~n" current-position character-index closing-pairs characters)
                ;(printf "~n~v~n" current-closing-pairs)
                

                (cond 
                    [(hash-has-key? current-closing-pairs character-index)
                        (let ([matching-bracket-index (hash-ref current-closing-pairs character-index)])
                            (highlight-pair character-index matching-bracket-index))]
                    [(hash-has-key? closing-pairs character-index)
                        (let ([matching-bracket-index (hash-ref closing-pairs character-index)])
                            (highlight-pair character-index matching-bracket-index))]
                    [else (when (hash-has-key? characters character-index)
                        (let* ([character (hash-ref characters character-index)]
                                [line (if (hash-has-key? lines (character-line-index character)) 
                                        (hash-ref lines (character-line-index character))
                                        (line current-characters current-row-number accumulated-character-index))])
                            (set! highlighted-indices '(-1 -1))
                            (set-cursor-row (line-row-number line))
                            (print-line (line-characters line) #f 0 (line-row-number line) 0)
                            (set-cursor-row current-row-number)
                        ))])))
                        #|
                        get the line row and offset
                        move cursor to there
                        insert highlight
                        redraw line
                        move cursor back
                        |#

        (define (get-matching-bracket bracket-to-match)
            (let ([bracket
                (if (> bracket-counter 0)
                    (begin 
                        (let ([length-colours (length current-line-bracket-colours)])
                            (findf (match-lambda
                                [(list bracket colour index) (are-matching? (list bracket bracket-to-match))])
                                        (drop current-line-bracket-colours 
                                            (clamp (sub1 (- length-colours bracket-counter))
                                                0 length-colours)))))
                    (begin 
                        (let ([length-colours (length used-bracket-colours)])
                            (findf (match-lambda 
                                [(list bracket colour index) (are-matching? (list bracket bracket-to-match))])
                                    (drop used-bracket-colours
                                        (clamp 
                                             (- length-colours (+ bracket-counter cumulative-bracket-counter))
                                            0
                                            length-colours))))))])
                (if bracket
                    bracket
                    #f)))
                            
        (define (lex lst acc character-index highlight-index)
            ;(printf "lex ci: ~v~n" character-index)
            (if (null? lst)
                (begin 
                    (set! current-character-index (- character-index accumulated-character-index))
                    acc)
                (match (first lst)
                    [(or #\( #\[)
                        (let* ([current-bracket-counter bracket-counter]
                                [colour (get-bracket-colour (+ cumulative-bracket-counter current-bracket-counter))])
                            (set! bracket-counter (add1 bracket-counter))
                            (set! current-line-bracket-colours (cons (list (first lst) colour character-index) current-line-bracket-colours))
                            (hash-set! characters character-index (character current-row-number #|line-index|# 0 (first lst)))
                            (let ([acc (add-to-acc acc (set-colour colour))])
                                ;(printf "ci: ~v hi: ~v eqv? ~v~n" character-index (first highlighted-indices)
                                (if (eqv? character-index (first highlighted-indices));highlight-index)
                                    (lex (rest lst)
                                        (add-to-acc (add-to-acc (add-to-acc acc highlight) (first lst)) normal)
                                        (add1 character-index) highlight-index)
                                    (lex (rest lst) (add-to-acc acc (first lst))
                                        (add1 character-index) highlight-index))))]
                    [(or #\) #\])
                        (set! closers (cons (first lst) closers))
                        (let* ([current-bracket-counter bracket-counter]
                                [matching-bracket (get-matching-bracket (first lst))]
                                [colour (if matching-bracket (second matching-bracket) invalid-bracket-colour)]
                                [acc (add-to-acc acc (set-colour colour))])
                            (set! bracket-counter (sub1 bracket-counter))
                            (hash-set! characters character-index (character current-row-number #|line-index|# 0 (first lst)))
                            (when matching-bracket
                                (begin
                                    (hash-set! current-closing-pairs (third matching-bracket) character-index)
                                    (hash-set! current-closing-pairs character-index (third matching-bracket))))

                            (set! current-line-bracket-colours (combine-and-match current-line-bracket-colours '() (list (first lst))))
                            (if (eqv? character-index (second highlighted-indices))
                                (lex (rest lst)
                                    (add-to-acc (add-to-acc (add-to-acc acc highlight) (first lst)) normal)
                                    (add1 character-index) highlight-index)
                                (lex (rest lst) (add-to-acc acc (first lst))
                                    (add1 character-index) highlight-index)))]
                    [(? char-numeric?)
                        (let-values ([(number remaining) (splitf-at lst char-numeric?)])
                            (let ([acc (add-to-acc acc (set-colour constant-colour))])
                                (lex remaining (add-to-acc acc number)
                                    (+ character-index (length number) highlight-index)
                                    highlight-index)))]
                    [(? char-whitespace?)
                        (hash-set! characters character-index (character current-row-number #|line-index|# 0 (first lst)))
                        (lex (rest lst) (add-to-acc acc (first lst))
                            (add1 character-index) highlight-index)]
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
                                    (lex remaining (add-to-acc acc quoted)
                                        (+ 1 (length string) (if add-closing-quote? 1 0)) highlight-index))))]
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
                                (for ([i identifier] [j (in-range (length identifier))])
                                    (hash-set! characters (+ character-index j) (character current-row-number #|line-index|# 0 i)))
                                (let ([acc (add-to-acc acc (set-colour colour))])
                                    (lex remaining (add-to-acc acc identifier)
                                        (+ (length identifier) character-index) highlight-index))))]

            )))
    ))



;TODO: replace with prompt from user profile
(define prompt-character 
    (match (system-type 'os)
        ['unix "λ "]
        ['windows "> "]
        ['macosx "λ "]
    ))

(define (refresh-line line current-position current-row-number [show-prompt? #t])
    (printf "\e[2K") ;ANSI escape code CSI n K - Erase in Line
    (printf "\e[1G") ;ANSI escape code CSI n G - Cursor Horizontal Absolute
    (when show-prompt?
        (display prompt-character))
    (display line)
    (printf "      row: ~v" current-row-number)
    (printf "\e[39;49m")
    (printf "\e[~aG" (+ (if show-prompt? 3 1) current-position))
    (flush-output)
    )