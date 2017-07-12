#lang racket/base

;todo: hide cursor before moving and rendering, show cursor afterwards
(provide (all-defined-out))

(require "escape-sequences.rkt"
    "dropdown.rkt"
    racket/class)

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
    (new dropdown% 
        [lines 
            (list
                "this is line 1"
                "this is line 2"
                "this is a super long line 3"
                "this is line 4")]))
    #|(listbox (random 5 15) (random 10 40)
        (for/list ([i (in-range 4)])
            (make-random-line 20 (integer->char (+ 49 i))))))|#

(define (draw-listbox box row column)
        #|(set-cursor-position row column)
        (for ([line (in-list (listbox-lines box))]
                [offset (in-naturals)])
            (set-highlight)
            (display (list->string (line-characters line)))
            (clear-highlight)
            (set-cursor-position (+ row offset 1) column))|#
    (send test-box draw row column 20)
    (move-cursor-to-current));)
