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

(provide screen%)

(require racket/class
    "bounding-box.rkt"
    "screen-buffer.rkt"
    "escape-sequences.rkt")

(struct cursor-position (row column))

(define screen%
    (class object%
        (init width height)
        (super-new)
        (define buffer (new screen-buffer% [width width] [height height]))
        (define current-cursor (cursor-position 1 1))
        (define widgets '())
        (define screen-height 20)

        (define/public (add-line line)
            (send buffer add-line line))

        (define/public (set-cursor-position row column)
            ;(printf "~n~n~n~nset column~a" column)
            (set! current-cursor (cursor-position row column)))

        (define/public (add-widget widget)
            (when (memq widget widgets)
                (send this remove-widget widget)
                (set! widgets (remq widget widgets)))

            (set! widgets (cons widget widgets))

            (send widget draw (cursor-position-row current-cursor)
                                (cursor-position-column current-cursor)
                                screen-height)
            (move-cursor (cursor-position-row current-cursor)
                        (cursor-position-column current-cursor)))

        (define/public (remove-widget widget)
            (when (memq widget widgets)
                (let* ([box (send widget get-bounding-box)]
                        ;[lines (send buffer get-lines-in-viewport box)]
                        [lines (send buffer get-lines box)]
                        [row (point-row (bounding-box-start box))]
                        [column (point-column (bounding-box-start box))])

                    (move-cursor row column)
                    
                    (for ([line (in-list lines)] [offset (in-naturals)])
                        (display line)
                        (move-cursor (+ row offset 1) column)))

                (move-cursor (cursor-position-row current-cursor)
                            (cursor-position-column current-cursor))))


    ))