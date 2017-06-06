#lang racket

(require ffi/unsafe)

(define ncurses (ffi-lib "libncursesw.so.5" '(#f)))
(define libc (ffi-lib #f))

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
(define wattron (get-obj "wattron" (_fun _pointer _int -> _void)))
(define wattroff (get-obj "wattroff" (_fun _pointer _int -> _void)))
(define COLOR_PAIR (get-obj "COLOR_PAIR" (_fun _int -> _int)))

;windows
(define newwin (get-obj "newwin" (_fun _int _int _int _int -> _pointer)))
(define getcurx (get-obj "getcurx" (_fun _pointer -> _int)))
(define getcury (get-obj "getcury" (_fun _pointer -> _int)))
(define wborder (get-obj "wborder" (_fun _pointer _uint8 _uint8 _uint8 _uint8 _uint8 _uint8 _uint8 _uint8 -> _int)))
(define keypad (get-obj "keypad" (_fun _pointer _int -> _void)))

;colour
(define has_colors (get-obj "has_colors" (_fun -> _int)))
(define start_color (get-obj "start_color" (_fun -> _void)))
(define init_pair (get-obj "init_pair" (_fun _int _int _int -> _void)))
(define init_color (get-obj "init_color" (_fun _int _int _int _int -> _void)))
(define wbkgd (get-obj "wbkgd" (_fun _pointer _int -> _void)))

;;;;;;;;;;;
(define signal (get-ffi-obj "signal" libc (_fun _int (_fun _int -> _void) -> _void)))

(define (wtf a)
    (display "wtf"))

(signal 2 wtf)
(signal 20 wtf)

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

;tab is 9?

;have ability to tap into other prog's stdout
;write a func that checks jekyll's out for a rebuild, play a sound
;have alert for when job finishes

(define (loop window r g b)
    ;(init_color r 0 0 b)
    (init_pair r 0 26)
    (wattron window (COLOR_PAIR r))
    (wprintw window "x")
    ;(wattroff window (COLOR_PAIR r))
    (wrefresh window)
    (when (< b 256)
        (loop window (+ r 1) g (+ 1 b)))
    )

(with-handlers ([exn:fail? (endwin)])

    (initscr)
    (start_color)
    ;(init_pair 1 222 3)

    (define window (newwin 80 80 0 0))
    (keypad window 1)
    ;(wattron window (COLOR_PAIR 1))

    ;(displayln (colors))
    (loop window 20 0 0)

    ;(define (c a) (char->integer a))
    ;(wborder window (c #\|) (c #\|)  (c #\-)  (c #\-)  (c #\+)  (c #\+)  (c #\+)  (c #\+))
    (wrefresh window)

    (wprintw window "test")
    (define x (getcurx window))
    (define line (ffi-read-line window))
    (wgetch window)
    (endwin))