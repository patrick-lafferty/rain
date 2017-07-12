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

(provide dropdown%)

(require racket/class
    "escape-sequences.rkt")

#|
A dropdown is a widget that displays a list of lines
either below a given row (if there is enough space 
between the line and the bottom of the screen), 
otherwise above the line
|#
(define dropdown% 
    (class object% 
        (init lines)
        (super-new)

        (define normalized-lines
            (let* ([lengths (map string-length lines)]
                    [max-length (apply max lengths)])
                (for/list ([line (in-list lines)] [line-length (in-list lengths)])
                    (let ([difference (- max-length line-length)])
                        (if (> difference 0)
                            (string-append line (make-string difference #\space))
                            line)))))

        (define number-of-lines (length lines))

        (define/public (draw row column terminal-height)
            (let ([row 
                (if (> terminal-height (+ row number-of-lines 1))
                    ;there's enough room to draw below the line
                    (add1 row)

                    ;have to draw above the line
                    (- row number-of-lines))])
                (set-cursor-position row column)
                (set-highlight)
                
                (for ([line (in-list normalized-lines)]
                        [offset (in-naturals)])
                    (display line)
                    (set-cursor-position (+ row offset 1) column))

                (clear-highlight)))

    ))