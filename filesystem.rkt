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

(require racket/system)

(provide 
    can-execute
    watch
    stop-watching
    is-watching?)

(require ffi/unsafe)
(require ffi/unsafe/define)
(require racket/place)

(define-ffi-definer define-libnotify (ffi-lib "libnotify" '(#f)))

(define-libnotify watch_path (_fun _string -> _int))


#|(define _cb (_cprocedure
    (list _int)
    _int
))|#

;(define-libnotify watch_all (_fun _cb -> _int))

;(define (watch func) (watch_all func))

(require "job-parameters.rkt")

(define (watch-th ch fn)
    (let ([r (sync ch)])
        (parameterize ([is-in-thread? #t])
            (fn r)))
    (watch-th ch fn))

(define (watch path func) 

    (watch_path "/home/pat/projects/lush/docs")

    ;(let ([p (place ch (watcher ch))])
    (let ([p (dynamic-place "filesystem-watcher-place.rkt" 'watcher)])
        (thread (lambda () (watch-th p func)))))
                

(define (can-execute path)
    (let ([filename 
            (cond 
                [(symbol? path) (symbol->string path)]
                [(string? path) path]
            )])
        (or (find-executable-path filename)
            (find-executable-path (format "~a.exe" filename)))))

#|(define (watcher path) 
    (filesystem-change-evt? 
        (sync 
            (choice-evt 
                (thread-receive-evt) 
                (filesystem-change-evt path (lambda () #f))))))
|#
(define watchers (make-hash))

#|(define (watch path func)
    (unless (hash-has-key? watchers path)
        (printf "watching ~v...~n" path)
        (let ([watcher-thread
                (thread
                    (lambda ()
                        (letrec ([f (lambda () 
                            (if (watcher path) 
                                (begin
                                    (displayln "changed") 
                                    (func)
                                    (f))
                                (displayln "thread-receive-evt")))])
                            (f))
                        (displayln "done")
                        ))])
            (hash-set! watchers path watcher-thread))))|#

(define (stop-watching path)
    (when (hash-has-key? watchers path)
        (thread-send (hash-ref watchers path) 1)
        (hash-remove! watchers path)))

(define (is-watching? path) (hash-has-key? watchers path))