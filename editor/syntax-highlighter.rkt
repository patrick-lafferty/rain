#lang racket/base

(provide 
    pretty-printer%)

(require 
    racket/list
    racket/class
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

(define pretty-printer%
    (class object%
        (super-new)
        (define previous-lines (make-empty-accumulated-lines))
        (define current-accumulated-lines (make-empty-accumulated-lines))
        (define current-line #f)
        (define indent 0)
        (define show-prompt? #t)
        (define cached-characters '())

        (define/public (print-line characters show-promptt? column row)
            (let-values ([(acc line lines)
                    (lex 
                        (clamp-line characters) 
                        '() 
                        0
                        (make-empty-saved-line (get-next-line-number current-accumulated-lines))
                        current-accumulated-lines)])
                        ;(displayln acc)
                (let* ([flattened (flatten acc)]
                        [string (list->string (reverse flattened))]
                        [indented (string-append (make-string (max 0 (+ (if (> indent 0) 2 0) indent)) #\space) string)])
                    ;(set! current-line line)
                    (set! cached-characters characters)
                    ;(set! current-accumulated-lines lines)
                    (write-line indented (+ (if (> indent 0) 2 0) indent column) show-prompt?))))

        (define/public (new-line)
            ;lex one last time to get the proper matches so there aren't
            ;any orphans in the previous lines
            (let-values ([(acc line lines)
                    (lex 
                        (clamp-line cached-characters) 
                        '() 
                        0
                        (make-empty-saved-line (get-next-line-number current-accumulated-lines))
                        current-accumulated-lines)])
                (set! current-line line)
                (set! current-accumulated-lines (add-line-to-accumulated current-line lines)))
                    
            (set! show-prompt? #f)
            (let ([bracket-counter (saved-line-bracket-counter current-line)])
                (cond
                    [(> bracket-counter 0)
                        (set! indent (+ indent 2))]
                    [(< bracket-counter 0)
                        (set! indent (- indent 2))])))

        (define/public (reset)
            (set! show-prompt? #t)
            (set! previous-lines (make-empty-accumulated-lines))
            (set! current-accumulated-lines (make-empty-accumulated-lines))
            (set! indent 0))

    ))