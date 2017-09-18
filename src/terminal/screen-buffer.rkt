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

(struct viewport (start end) #:transparent)

(define screen-buffer%
    (class object%
        (init width height)
        (super-new)
        #|
        lines stores the complete history of lines entered up to
        some limit, maybe unlimited

        we can only display a certain number of rows at a time,
        which is stored in viewport
        |#
        (define screen-width width)
        (define screen-height height)
        (define lines (make-list height (make-string width #\space)))
        (define current-viewport (viewport screen-height 0))

        (define/public (add-line line)  
            (set! lines (cons (pad-line line screen-width) lines))
            (set! current-viewport (struct-copy viewport current-viewport
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
            (let* ([line-count (length lines)]
                    [start-row (- (viewport-start viewport) (- line-count (point-row (bounding-box-start bounds))))]
                    [end-row (- (viewport-start viewport) (- line-count (point-row (bounding-box-end bounds))))])
                    ;(printf "~nline-count: ~v~nbounds: ~v~n" line-count bounds);bounds: (2 1) (6 28)
                (values start-row end-row)))

        (define/public (get-lines bounds)
            (let* ([lines 
                    (take (drop lines (- screen-height (point-row (bounding-box-end bounds)))) 
                        (- (point-row (bounding-box-end bounds)) (point-row (bounding-box-start bounds))))]
                   [bounded-lines (map 
                        (lambda (l) 
                            (substring l (sub1 (point-column (bounding-box-start bounds)))
                                        (point-column (bounding-box-end bounds))))
                        lines)])
                (reverse bounded-lines)))

        (define/public (get-lines-in-viewport bounds)
            (void))
            #|;(printf "~nlines: ~v~nviewport: ~v~n" lines current-viewport);("\ augment") and 1 and 0?
            (let-values ([(start-row end-row) (local-to-world bounds current-viewport)])
                ;(printf "~nstart-row: ~vend-row: ~v~n" start-row end-row);2 and 6?
                (let* ([lines (reverse (take lines (viewport-start current-viewport)))]
                    [bounded-lines (take (drop lines (sub1 start-row)) (- end-row start-row))]
                      [starting-column (point-column (bounding-box-start bounds))]
                      [ending-column (point-column (bounding-box-end bounds))]
                      [width ending-column])
                    (for/list ([line (in-list bounded-lines)])
                        (substring (pad-line line width) starting-column ending-column)))))
                        |#
    ))
