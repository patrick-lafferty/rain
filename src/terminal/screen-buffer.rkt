#|
MIT License
Copyright (c) 2017 Patrick Lafferty
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
#lang racket/base

(provide screen-buffer%)

(require racket/class
    racket/list
    "bounding-box.rkt")

(struct line (characters))

(struct viewport (start end))

(define screen-buffer%
    (class object%
        (init height)
        (super-new)
        #|
        lines stores the complete history of lines entered up to
        some limit, maybe unlimited

        we can only display a certain number of rows at a time,
        which is stored in viewport

        a
        b
        c <-- start
        d
        e <-- end
        f first


        |#
        (define screen-height height)
        (define lines '())
        (define current-viewport (viewport 0 0))

        (define/public (add-line line)  
            (set! lines (cons line lines))
            (set! current-viewport (struct-copy viewport current-viewport
                ;[start (min (length lines) (add1 (viewport-start current-viewort)))])))
                ;[start (min screen-height (add1 (viewport-start current-viewport)))])))
                [start (min screen-height (length lines))])))
                
        (define (pad-rows lines height width)
            (if (< (length lines) height)
                (for/fold ([acc lines]) ([i (in-range (- height (length lines)))])
                    (cons (make-string width #\space) acc))
                lines))
            
        (define (pad-line line width)
            (let ([length (string-length line)])
                (if (< length width)
                    (string-append line (make-string (- width length) #\space))
                    line)))

        (define (local-to-world bounds viewport)
            #|(let* ([start-row 
                    (+ (point-row (bounding-box-start bounds))
                            (viewport-start viewport))]
                [end-row 
                    (+ (point-row (bounding-box-end bounds))
                            (viewport-start viewport))]
                [capped-start (min screen-height start-row)]
                [capped-end (max 0 (min (length lines) end-row))])
                (values capped-start capped-end)))|#
            ;(printf "~nBB: ~v LL: ~v~n" bounds (length lines))
            (let* ([line-count (length lines)]
                    [start-row (- (viewport-start viewport) (- line-count (point-row (bounding-box-start bounds))))]
                    [end-row (- (viewport-start viewport) (- line-count (point-row (bounding-box-end bounds))))])
                (values start-row end-row)))

        #|(define/public (get-lines-in-box bounds)
            ;bounding box is local to viewport
            (let-values ([(start-row end-row) (local-to-world bounds current-viewport)])
                (let* (
                      [height start-row];(- (point-row (bounding-box-end bounds)) (point-row (bounding-box-start bounds)))]
                      [starting-column (point-column (bounding-box-start bounds))]
                      [ending-column (point-column (bounding-box-end bounds))]
                      [width ending-column]
                      [lines (take (drop (pad-rows lines height width) end-row) (- start-row end-row))])
                    (for/list ([line (in-list (pad-rows lines height width))])
                        (substring (pad-line line width) starting-column ending-column)))))
                        |#

        (define/public (get-lines-in-viewport bounds)
            (let-values ([(start-row end-row) (local-to-world bounds current-viewport)])
                ;(printf "~nSTART: ~v END: ~v~n" start-row end-row)
                ;(printf "RTL: ~v~n" (length (reverse (take lines (viewport-start current-viewport)))))
                (let* ([lines (reverse (take lines (viewport-start current-viewport)))]
                    [bounded-lines (take (drop lines (sub1 start-row)) (- end-row start-row))]
                      [starting-column (point-column (bounding-box-start bounds))]
                      [ending-column (point-column (bounding-box-end bounds))]
                      [width ending-column])
                    (for/list ([line (in-list bounded-lines)])
                        (substring (pad-line line width) starting-column ending-column)))))
    ))

#|

widget draws at an existing row, col and either expands down to a non-existing row
or up to a previous row

if its at row 30 it always draws at row 30

tail

.
. -------------------- above this line is off screen
. viewport end screen-height

head <- index 0, bounds start here at row 30, viewport start 0

dropdown start

dropdown end

dropdown::draw is given some arbitrary row
-convert to viewport bounds of [0, screen height)

absolute row index increases downwards, but viewport increases upwards?

it should be viewort increases downards too

viewport start is an absolute row index, to get local you should (x - viewport start)

|#