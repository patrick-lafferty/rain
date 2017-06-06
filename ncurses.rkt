#lang racket

(require ffi/unsafe)

(define ncurses (ffi-lib "libncurses.so.5" '(#f)))

(define (get-obj name signature)
    (get-ffi-obj name ncurses signature))

(define initscr (get-obj "initscr" (_fun -> _void)))
(define printw (get-obj "printw" (_fun _string -> _void)))
(define wprintw (get-obj "wprintw" (_fun _pointer _string -> _void)))
(define mvwprintw (get-obj "mvwprintw" (_fun _pointer _int _int _string -> _void)))
(define refresh (get-obj "refresh" (_fun -> _void)))
(define wrefresh (get-obj "wrefresh" (_fun _pointer -> _void)))
(define getch (get-obj "getch" (_fun -> _int)))
(define wgetch (get-obj "wgetch" (_fun _pointer -> _int)))
(define endwin (get-obj "endwin" (_fun -> _void)))

;windows
(define newwin (get-obj "newwin" (_fun _int _int _int _int -> _pointer)))
(define getcurx (get-obj "getcurx" (_fun _pointer -> _int)))
(define getcury (get-obj "getcury" (_fun _pointer -> _int)))
(define wborder (get-obj "wborder" (_fun _pointer _uint8 _uint8 _uint8 _uint8 _uint8 _uint8 _uint8 _uint8 -> _int)))

;;;;;;;;;;;

(define (ffi-read-line window)
    (define (helper acc)
        (let ([c (wgetch window)])
            (if (and
                    (> c -1)
                    (not (eq? c 10)))
                (helper (cons c acc))
                acc)))
    (let ([line (helper '())])
        (let ([reversed (reverse (map (lambda (i) (integer->char i)) line))])
            (list->string reversed))))

(initscr)
(define window (newwin 30 30 0 0))
(define (c a) (char->integer a))
(wborder window (c #\|) (c #\|)  (c #\-)  (c #\-)  (c #\+)  (c #\+)  (c #\+)  (c #\+))
(wrefresh window)
(wprintw window "test")
(define x (getcurx window))
(define line (ffi-read-line window))
(wgetch window)
(endwin)