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
        (super-new)
        #|
        lines stores the complete history of lines entered up to
        some limit, maybe unlimited

        we can only display a certain number of rows at a time,
        which is stored in viewport
        |#
        (define lines '())
        (define current-viewport (viewport 0 0))

        (define/public (add-line line)  
            (set! lines (cons line lines)))

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

        (define/public (get-lines-in-box bounds)
            ;bounding box is local to viewport
            (let-values ([(start-row end-row) (local-to-world bounds current-viewport)])
                (let* ([lines (take (drop lines start-row) (- end-row start-row))]
                      [starting-column (point-column (bounding-box-start bounds))]
                      [ending-column (point-column (bounding-box-end bounds))]
                      [width ending-column];(- ending-column starting-column)]
                      [height (- (point-row (bounding-box-end bounds)) (point-row (bounding-box-start bounds)))])
                    (for/list ([line (in-list (pad-rows lines height width))])
                        (substring (pad-line line width) starting-column ending-column)))))
                        ;(take (drop (pad-line line width) starting-column) width)))))
    ))

(define (local-to-world bounds viewport)
    (let* ([start-row 
               (+ (point-row (bounding-box-start bounds))
                    (viewport-start viewport))]
           [end-row 
               (+ (point-row (bounding-box-end bounds))
                    (viewport-start viewport))]
           [capped-start (max 0 (min (viewport-end viewport) start-row))] 
           [capped-end (max 0 (min (viewport-end viewport) end-row))])
        (values capped-start capped-end)))