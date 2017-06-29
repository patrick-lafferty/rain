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
(require racket/list)
(require racket/match)

(define-ffi-definer define-libnotify (ffi-lib "libnotify" '(#f)))

(define-libnotify watch_path (_fun _string -> _int))
(define-libnotify stop_watching_path (_fun _int -> _int))
(define-libnotify stop_watching(_fun -> _int))

(require "job-parameters.rkt")

(define (watch-thread channel fn my-descriptor)
    ;(let* ([choice (choice-evt (wrap-evt (thread-receive-evt) (lambda (x) (thread-receive))) channel)]
    ;        [result (sync choice)])
    (let ([result (sync (wrap-evt (thread-receive-evt) (lambda (x) (thread-receive))))])
        (printf "[watch-thread] ~v~n" result)
        (if (descriptor? result)
            (stop_watching_path (descriptor-d result))
            (begin 
                (when (eqv? my-descriptor result)
                    (parameterize ([is-in-thread? #t])
                        (fn result)))
                (watch-thread channel fn my-descriptor)))))

;todo: cancel watches
;dont hardcode path in notify.cpp
;only one place for all watches

(define watcher-place #f)

(struct watcher (descriptor thread))

(struct subscribe (thread))
(struct unsubscribe (thread))

(define (master-thread place_channel subscribers) 
    (let* ([choice (choice-evt (wrap-evt (thread-receive-evt) (lambda (x) (thread-receive))) place_channel)]
            [result (sync choice)])
        (cond
            [(integer? result)
                (begin 
                    (for ([i subscribers])
                        (thread-send i result))
                    (master-thread place_channel subscribers))]
            [(subscribe? result)
                (master-thread place_channel (cons (subscribe-thread result) subscribers))]
            [(unsubscribe? result)
                (master-thread place_channel (remove (unsubscribe-thread result) subscribers))]
            [else (void)])))
            
(define master #f)

(define (watch path func) 

    (let ([watch-descriptor (watch_path path)]);"/home/pat/projects/lush/docs")])

        (unless watcher-place
            (displayln "creating watcher-place")
            (set! watcher-place (dynamic-place "filesystem-watcher-place.rkt" 'watcher))
            (set! master (thread (lambda () (master-thread watcher-place '())))))

        (let ([new-thread (thread (lambda () (watch-thread watcher-place func watch-descriptor)))])
            (thread-send master (subscribe new-thread))
            (hash-set! watchers path (watcher watch-descriptor new-thread)))))

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

(struct descriptor (d))

(define (stop-watching path)
    (when (hash-has-key? watchers path)
        (let ([watcher (hash-ref watchers path)])
            (thread-send (watcher-thread watcher) (descriptor (watcher-descriptor watcher)));(watcher-descriptor watcher))
            (thread-send master (unsubscribe (watcher-thread watcher)))
            (hash-remove! watchers path)
            (when (hash-empty? watchers)
                (stop_watching)
                (thread-send master #f)
                (place-kill watcher-place)
                (set! watcher-place #f)))))

(define (is-watching? path) (hash-has-key? watchers path))