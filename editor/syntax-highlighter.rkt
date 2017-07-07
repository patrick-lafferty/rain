#lang racket/base

(provide 
    pretty-printer%)

(require 
    racket/list
    racket/class
    racket/match
    (except-in "lexer.rkt" flatten)
    "../terminal.rkt"
    "../env.rkt")

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

(define (expand-highlight 
            acc
            characters) 
    (let* (
            [unhighlight (foldl cons acc (reverse (string->list "\e[0m")))]
            [characters (foldl cons unhighlight characters)]
            [highlighted (foldl cons characters (reverse (string->list "\e[48;5;183m")))]
            )
        highlighted))

(define (should-highlight? 
            line-index 
            character-index
            highlighted) 
    (let ([pair (matching-pair line-index character-index)])
        (or (equal? pair (highlighted-pair-first highlighted))
            (equal? pair (highlighted-pair-second highlighted)))))


(define (expand lexed current-highlighted-pair)
    (reverse 
        (for/fold ([acc '()]) ([i lexed])
            (match i
                [(highlight-point line-index character-index characters) 
                    #|(for/fold ([acc acc]) ([j x])
                        (cons j acc))]|#
                        (if (should-highlight? line-index character-index current-highlighted-pair)
                            (expand-highlight acc characters)
                            (for/fold ([acc acc]) ([j characters])
                                (cons j acc)))]
                [(autocomplete-point x) 
                    ;(cons x acc)]
                    (for/fold ([acc acc]) ([j x])
                        (cons j acc))]
                [ (? list?)
                    (for/fold ([acc acc]) ([j i])
                        (cons j acc))]
                [_ (cons i acc)]

            )))
            )
                
                
                
                ;(cons i acc)]))))

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

        (define (do-print acc indent column show-prompt?);characters column row)
                ;(printf "~n~v~n" acc)
                (let* ([expanded (expand acc highlighted)]
                        [flattened (flatten expanded)]
                        [string (list->string (reverse flattened))]
                        [indented (string-append (make-string (max 0 (- indent column)) #\space) string)])
                    (write-line indented indent show-prompt?)))
;                    (values line characters))))

        (define/public (print-line characters show-promptt? column row)
            ;(when (< current-row 0) (set! current-row row))
            (when show-prompt? (set! current-row row))

            (let-values ([(acc line lines)
                    (lex 
                        (clamp-line characters) 
                        '() 
                        0
                        (make-empty-saved-line (get-next-line-number current-accumulated-lines))
                        current-accumulated-lines
                        highlighted)])
            ;(let-values ([(line characters) (do-print characters column (get-next-line-number current-accumulated-lines))])
                (do-print acc (+ (if (> indent 0) 2 0) indent column) column show-prompt?)
                (set! current-line line)
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
                    ;(displayln acc)
                    (set! current-accumulated-lines (add-line-to-accumulated line lines))))
                    
            (set! show-prompt? #f)
            (let ([bracket-counter (saved-line-bracket-counter current-line)])
                (cond
                    [(> bracket-counter 0)
                        (set! indent (+ indent 2))]
                    [(< bracket-counter 0)
                        (set! indent (- indent 2))])))

        (define/public (reset)
            (set! current-row -1)
            (set! show-prompt? #t)
            (set! current-accumulated-lines (make-empty-accumulated-lines))
            (set! indent 0))


        (define (highlight line-index)
            (let ([line (findf 
                            (lambda (line) (eqv? (saved-line-index line) line-index))
                            (accumulated-lines-lines current-accumulated-lines))])
                ;(unless line (printf "~n~n~n~n~n~n~n~n~nNOPE~n~n~n~n~n~n~n~n~n"))
                ;(printf "~n~n~n~n~n~n~n~n~n~v~n~n~n~n~n" (+ current-row 0 line-index))
                (if line
                    (begin
                        ;(printf "~n~n~n~n~n~n~n~n~nONE~n~n~n~n~n~n~n~n~n")
                        (printf "\e[~a;H" (+ current-row 0 line-index))
                        ;(flush-output)
                        ;(displayln line)
                        ;(printf "~n~n~n~n~n~n~n~n~n~n~v~n~n~n~n~n~n~n~n" (saved-line-lexed line))
                        (do-print (saved-line-lexed line) (saved-line-indent line) 0 (eqv? line-index 0)))

                    (when (equal? line-index (saved-line-index current-line))
                        ;(printf "~n~n~n~n~n~n~n~n~nTWO~n~n~n~n~n~n~n~n~n")
                        (printf "\e[~a;H" (+ current-row 0 line-index))
                        ;(printf "\e[22;H"); (+ current-row 5 line-index))
                        ;(flush-output)
                        (do-print (saved-line-lexed current-line) (saved-line-indent current-line) 0 (eqv? line-index 0))))))
                    ;)))


                    ;(do-print (saved-line-characters line) 0 line-index))))
                    #|(printf "\e[~aG" character-index)
                    (printf "\e[48;5;183m")
                    (display "("))))|#

        (define (highlight-pair key line matched-pair)
            (if (integer? key)
                (begin
                    (set! highlighted (highlighted-pair (matching-pair line key) matched-pair))
                    ;(printf "~n~n~n~n~n~n~n~n~n~n~n~v~n~n~n~n~n~n~n~n~n~n" highlighted)
                    (let ([current-line-index line] ;(saved-line-index current-line)]
                           [first-line-index (matching-pair-line (highlighted-pair-first highlighted))]
                           [second-line-index (matching-pair-line (highlighted-pair-second highlighted))])
                        ;(when (not (equal? current-line-index first-line-index))
                            ;(highlight first-line-index)
                            ;(flush-output)
                            (highlight second-line-index)
                            (printf "\e[~a;H" (+ current-row current-line-index))
                    ;(flush-output)
                        ;(printf "~n~n~n~n~n~n~n~n~ncur: ~v fir: ~v sec: ~v~n~n~n~n~n" current-row first-line-index second-line-index)

                        #|(when (not (equal? current-line-index second-line-index))
                            (highlight second-line-index)
                            (printf "\e[~a;H" (add1 current-line-index))|#
                    ;(flush-output)
                            ))
                (void)
            ))

        (define (remove-old-highlight high)
            (let ([first (highlighted-pair-first high)]
                    [second (highlighted-pair-second high)])
                (highlight (matching-pair-line first))
                (highlight (matching-pair-line second))))

        (define/public (highlight-matching-bracket column)
            (unless (is-highlighted-empty? highlighted)
                (let ([copy (struct-copy highlighted-pair highlighted)])
                    (set! highlighted (make-empty-highlighted-pair))
                    (remove-old-highlight copy)
                    (printf "\e[~a;H" (+ current-row (saved-line-index current-line)))
                    ))

            (when current-line
                (let ([matching-pairs (saved-line-matching-pairs current-line)]
                        [offset (- column 0)]); (+ (if (> indent 0) 2 0) indent))])
                    (if (hash-has-key? matching-pairs offset)
                        (highlight-pair offset (saved-line-index current-line) (hash-ref matching-pairs offset))
                        (if (hash-has-key? matching-pairs (foreign-key offset))
                            (void)
                            (void))))))

    ))