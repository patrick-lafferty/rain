#lang racket/base

(provide (all-defined-out))

;todo: hide cursor before moving and rendering, show cursor afterwards

(define (enter-cursor-address-mode)
    (printf "\e[?1049h"))

(define (exit-cursor-address-mode)
    (printf "\e[?1049l"))

(struct cursor-position (row column))

(define current-cursor (cursor-position 1 1))

(define (set-current-row! row)
    (set! current-cursor 
        (struct-copy cursor-position current-cursor
            [row row])))

(define (set-current-column! column)
    (set! current-cursor 
        (struct-copy cursor-position current-cursor
            [column column])))

(struct line (characters))
(struct buffer (lines))

(struct listbox (row column lines))

(define (make-random-line [length 100] [first #\1])
    (line (cons first
        (for/list ([i (in-range length)])
            (integer->char (random 97 122))))))

(define (make-random-buffer)
    (buffer 
        (for/list ([i (in-range 25)])
            (make-random-line))))

(define random-buffer (make-random-buffer))

(define (set-cursor-row row)
    (printf "\e[~a;H" row))

(define (set-cursor-position row column)
    (printf "\e[~a;~aH" row column))

(define (move-cursor-to-current)
    (set-cursor-position 
        (cursor-position-row current-cursor)
        (cursor-position-column current-cursor)))

(define (fill-screen)
    (set-cursor-row 1)
    (for ([i (buffer-lines random-buffer)] [j (in-naturals)])
        (display (list->string (line-characters i)))
        (set-cursor-row (add1 j))))

(define test-box 
    (listbox (random 5 15) (random 10 40)
        (for/list ([i (in-range 4)])
            (make-random-line 20 (integer->char (+ 49 i))))))

(define (set-highlight)
    (printf "\e[38;5;0m")
    (printf "\e[48;5;183m"))

(define (clear-highlight)
    (printf "\e[0m"))

(define (draw-listbox box row column)
    #|(let ([row (listbox-row box)]
            [column (listbox-column box)])|#
        (set-cursor-position row column)
        (for ([line (in-list (listbox-lines box))]
                [offset (in-naturals)])
            (set-highlight)
            (display (list->string (line-characters line)))
            (clear-highlight)
            (set-cursor-position (+ row offset 1) column))
    (move-cursor-to-current));)
