#lang racket/base

;todo: hide cursor before moving and rendering, show cursor afterwards
(provide widget%)

(require "escape-sequences.rkt"
    ;"dropdown.rkt"
    "bounding-box.rkt"
    racket/class)

(define widget%
    (class object%
        (super-new)

        (define bounding-box (make-empty-bounding-box))

        (define/public (set-bounding-box box)
            (set! bounding-box box))

        (define/public (get-bounding-box) bounding-box)
    ))

#|
(define (set-current-row! row)
    (set! current-cursor 
        (struct-copy cursor-position current-cursor
            [row row])))

(define (set-current-column! column)
    (set! current-cursor 
        (struct-copy cursor-position current-cursor
            [column column])))

(struct buffer (lines))

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

(define (draw-listbox box row column)
    (send test-box draw row column 20)
    (move-cursor-to-current))
|#