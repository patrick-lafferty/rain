#lang racket/base

;the module deals with the user input from the shell's command line

(provide 
 ;class that handles basic input editing
 commandline%)

(require racket/list)
(require racket/class)

(define (erase-at lst i)
    (define (helper c before after)
        (if (= c i)
            (append (reverse before) (rest after))
            (helper (+ c 1) (cons (first after) before) (rest after))))
    (let ([len (length lst)])
            (when (and (< i len)
                    (> i -1)) 
                (helper 0 '() lst))))

(define (insert-at lst i element)
    (define (helper c before after)
            (if (= c i)
                (append (reverse before) (list element) after)
                (helper (+ c 1) (cons (first after) before) (rest after))))
        (let ([len (length lst)])
                (when (and (< i len)
                        (> i -1)) 
                    (helper 0 '() lst))))

(define commandline%
    (class object%
        (super-new)

        (define position 0)
        (define line '())
        (define multiline '())
        (define length 0)

        (define/public (is-in-multiline?) (not (null? multiline)))

        (define/public (store)
            (set! multiline (cons line multiline)))

        (define/public (clear)
            (clear-single)
            (set! multiline '()))

        (define/public (clear-single)
            (set! position 0)
            (set! line '())
            (set! length 0))

        (define/public (set-from-history past-line)
            (set! line (reverse (string->list past-line)))
            (set! length (string-length past-line))
            (set! position length))

        (define/public (add-char c)
            (if (< position length)
                ;insert somewhere in the middle of the line
                (begin
                    (set! line (reverse (insert-at (reverse line) position c)))
                    (set! position (+ 1 position))
                    (set! length (+ 1 length))
                )
                ;add to the end of the line
                (begin 
                    (set! line (cons c line))
                    (set! position (+ 1 position))
                    (set! length (+ 1 length)))))

        (define/public (get-position) position)
        (define/public (get-length) length)

        (define (update-cursor)
            (printf "\x1b[~aG" (+ 3 position))
            (flush-output))

        (define/public (move-left)
            (when (> position 0)
                (set! position (- position 1))
                (update-cursor)))

        (define/public (move-right)
            (when (< position length)
                (set! position (+ position 1))
                (update-cursor)))

        (define/public (delete)
            (when (and
                    (< position length)
                    (> position -1))
                (set! line (reverse (erase-at (reverse line) position)))
                (set! length (- length 1))))

        (define/public (backspace)
            (when (and 
                    (> position 0)
                    (> length 0))
                (set! line (reverse (erase-at (reverse line) (- position 1))))
                (set! length (- length 1))
                (set! position (- position 1))))

        (define/public (get-line-single) (list->string (reverse line)))

        (define/public (get-line) 
            (if (null? multiline)
                (list->string (reverse line))
                (let ([folded-list (flatten (cons line multiline))])
                    (list->string (reverse folded-list)))))

    ))
