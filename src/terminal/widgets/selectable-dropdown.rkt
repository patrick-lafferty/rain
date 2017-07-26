#lang racket/base

(provide selectable-dropdown%)

(require racket/class
    racket/list
    "dropdown.rkt"
    "../bounding-box.rkt"
    "../escape-sequences.rkt")

(define selectable-dropdown% 
    (class dropdown%
        (inherit set-bounding-box)
        (inherit-field normalized-lines number-of-lines)
        (init lines)
        (super-new [lines lines])
        (define selected-index 0)
        (define max-lines-to-draw 5)

        (define/public (select-up)
            (set! selected-index (max 0 (sub1 selected-index))))

        (define/public (select-down)
            (set! selected-index (min (sub1 number-of-lines) (add1 selected-index))))

        (define (before-line index)
            (if (= index selected-index)
                (set-highlight 27)
                (set-highlight)))

        (define (get-lines-to-draw)
            (let ([line-count number-of-lines])
                (if (> line-count max-lines-to-draw)
                    (if (> (- line-count selected-index) max-lines-to-draw)
                        (values 0 
                            (take (drop normalized-lines (- line-count selected-index max-lines-to-draw)) max-lines-to-draw))
                        (values (- line-count selected-index) (take normalized-lines max-lines-to-draw)))
                    (values selected-index normalized-lines))))

        (define/override (draw row column terminal-height)
            (let* ([lines-to-draw (min number-of-lines max-lines-to-draw)]
                [row 
                (if (> terminal-height (+ row lines-to-draw 1))
                    ;there's enough room to draw below the line
                    (add1 row)

                    ;have to draw above the line
                    (- row lines-to-draw))])
                (move-cursor row column)
                (set-highlight)

                (let-values ([(index lines) (get-lines-to-draw)])
                    (for ([line (in-list lines)]
                            [offset (in-naturals)])
                        (if (= index offset)
                            (set-highlight 27)
                            (set-highlight))
                        (display line)
                        (move-cursor (+ row offset 1) column)))

                (clear-highlight)
                (set-bounding-box 
                    (bounding-box 
                        (point row column)
                        (point (+ row lines-to-draw) 
                            (+ column (string-length (first normalized-lines))))))
                ))

    ))