#lang racket/base

(provide watcher)

(require ffi/unsafe)
(require ffi/unsafe/define)
(require racket/place)

(define-ffi-definer define-libnotify (ffi-lib "libnotify" '(#f)))

(define-libnotify watch_all (_fun (length : (_ptr o _int))
    -> (buffer : (_cpointer 'buffer))
    -> (values length buffer)))

(define (watcher ch)
    (let-values ([(length buffer) (watch_all)])
        (when (> length 0)
            (for ([i length])
                (place-channel-put ch (ptr-ref buffer _int i))))

        (free buffer))
    (watcher ch))