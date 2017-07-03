#lang racket/base

(provide print-colour-syntax)

(require racket/match)
(require racket/list)

(define bracket-counter 0)
(define (inc-brackets) (set! bracket-counter (add1 bracket-counter)))
(define (dec-brackets) 
    (unless (eq? bracket-counter 0)
        (set! bracket-counter (sub1 bracket-counter))))

(define bracket-colours #(68 100 160))
(define (get-bracket-colour x) (vector-ref bracket-colours (modulo x (vector-length bracket-colours))))

(define (set-colour x)
    ;())
    (string->list (format "\e[38;5;~am" x)))

(define constant-colour 82)
(define identifier-colour 117)

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
    ;(printf "acc: ~v thing: ~v~n" acc thing)
    (if (null? acc)
        thing
        (cons acc thing)))

(define (lex lst acc)
    (if (null? lst)
        acc
        (match (first lst)
            [(or #\( #\[)
                (let ([current-bracket-counter bracket-counter])
                    (inc-brackets)
                    (let ([acc (add-to-acc acc (set-colour (get-bracket-colour current-bracket-counter)))])
                        (lex (rest lst) (add-to-acc acc (first lst)))))]
            [(or #\) #\])
                (dec-brackets)
                (let ([acc (add-to-acc acc (set-colour (get-bracket-colour bracket-counter)))])
                    (lex (rest lst) (add-to-acc acc (first lst))))]
            [(? char-numeric?)
                (let-values ([(number remaining) (splitf-at lst char-numeric?)])
                    (let ([acc (add-to-acc acc (set-colour constant-colour))])
                        (lex remaining (add-to-acc acc number))))]
            [(? char-whitespace?)
                (lex (rest lst) (add-to-acc acc (first lst)))]
            [_ 
                (let-values ([(identifier remaining) (splitf-at lst char-identifier?)])
                    (let ([acc (add-to-acc acc (set-colour identifier-colour))])
                        (lex remaining (add-to-acc acc identifier))))]

        )))


(define (print-colour-syntax lst show-prompt?)
    (set! bracket-counter 0)
    (let* ([lexed (lex lst '())]
            [line (list->string (flatten lexed))])
        ;(printf "lexed: ~v~nline:~v~n" lexed line)
        (refresh-line line show-prompt?)))
        ;(display line)))

;TODO: replace with prompt from user profile
(define prompt-character 
    (match (system-type 'os)
        ['unix "λ "]
        ['windows "> "]
        ['macosx "λ "]
    ))

(define (refresh-line line [show-prompt? #t])
    ;(display "~nprinting: ~v~n" line)
    (printf "\e[2K") ;ANSI escape code CSI n K - Erase in Line
    (printf "\e[1G") ;ANSI escape code CSI n G - Cursor Horizontal Absolute
    ;(when show-prompt?
    ;    (display prompt-character))
    (display line);(send commandline get-line-single))
    ;(display "a")
    ;(printf "\x1b[~aG" (+ (if show-prompt? 3 1) (send commandline get-position)))
    (flush-output)
    )