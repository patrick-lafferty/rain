#lang racket

(require ffi/unsafe)

(define libc (ffi-lib #f))

(define fork (get-ffi-obj "fork" libc (_fun -> _int)))
(define execvp (get-ffi-obj "execvp" libc (_fun _path (_list i _string) -> _int)))
(define wait (get-ffi-obj "wait" libc (_fun _pointer -> _void)))

(define getpid (get-ffi-obj "getpid" libc (_fun -> _int)))
(define setpgid (get-ffi-obj "setpgid" libc (_fun _int _int -> _int)))

(define c-exit (get-ffi-obj "exit" libc (_fun _int -> _void)))
(define signal (get-ffi-obj "signal" libc (_fun _int (_fun _int -> _void) -> _void)))

(define (sigttou a) (void))

(define (sigttin a) (void))

(define (a b) (printf "caught ~a~n" b))
(signal 1 a)
(signal 2 a)
(signal 3 a)
(signal 4 a)
(signal 6 a)
(signal 8 a)
;(signal 11 a)
(signal 13 a)
(signal 15 a)
(signal 18 a)

;(signal 21 sigttin)
(signal 22 sigttou)

(require "terminal.rkt")
(require "termios.rkt")

(define job%
    (class object%
        (super-new)
        (define termios (new termios%))

        (define can-continue? #f)
        (define pgid 0)
        ;(define lock (make-semaphore 1))

;        (define/public (get-semaphore) lock)

        (define/public (become-foreground-process terminal)
            (when can-continue?
                (send termios restore-tmodes terminal))

            (set-foreground-process-group terminal pgid))

        (define/public (stop terminal)
            (send termios save-tmodes terminal)
            (set! can-continue? #t))

        (define/public (get-pgid) 
            pgid)
        (define/public (set-pgid id)
            ;(printf "before set: ~a~n" pgid)
            (set! pgid id))
    ))

(define semaphore (make-semaphore 1))
(define (set-job-pgid job pgid)
    (semaphore-wait semaphore)
    (when (= 0 (send job get-pgid))
        ;(displayln "setting job pgid")
        ;(printf "jobpgid: ~a ~n" (= 0 (send job get-pgid)))   
        (send job set-pgid pgid) 
        (setpgid pgid pgid))
    (semaphore-post semaphore))

(define launcher%
    (class object%
        (super-new)
        (init current-shell)
        (define shell current-shell)

        (define/public (launch-process job)
            (define pid (getpid))

            (set-job-pgid job pid)

            (send job become-foreground-process (send shell get-terminal))
            (execvp "python" '("python" #f))
            ;(send job stop (send shell get-terminal))
            (c-exit 1))

        (define/public (put-job-in-foreground job pid)
            (set-job-pgid job pid)

            (define terminal (send shell get-terminal))
            (send job become-foreground-process terminal)
            (wait #f)
            ;(send job stop terminal)
            (displayln "becoming foreground")
            (send shell become-foreground-process)
            
            (void))

        (define/public (launch-job job)
            (define pid (fork))
            (if (eq? pid 0)
                (launch-process job)
                (put-job-in-foreground job pid)))))

(provide launcher% job%)