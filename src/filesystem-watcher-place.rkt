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

(provide create-watcher-place)

(require ffi/unsafe)
(require ffi/unsafe/define)
(require racket/place)

(define libnotify-path (build-path (find-system-path 'collects-dir) "libnotify"))
(define-ffi-definer define-libnotify (ffi-lib libnotify-path '(#f)))

(define-libnotify watch_all (_fun (length : (_ptr o _int))
    -> (buffer : (_cpointer 'buffer))
    -> (values length buffer)))

(define (watcher channel)
    (let-values ([(length buffer) (watch_all)])
        (when (> length 0)
            (for ([i length])
                (place-channel-put channel (ptr-ref buffer _int i))))

        (free buffer))
    (watcher channel))

(define (create-watcher-place)
    (place channel (watcher channel)))