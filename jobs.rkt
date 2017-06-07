#lang racket

(require ffi/unsafe)

(define libc (ffi-lib #f))

(define fork (get-ffi-obj "fork" libc (_fun -> _int)))
(define execvp (get-ffi-obj "execvp" libc (_fun _path (_list i _string) -> _int)))
(define wait (get-ffi-obj "wait" libc (_fun _pointer -> _void)))

(define getpgrp (get-ffi-obj "getpgrp" libc (_fun -> _int)))
(define tcgetpgrp (get-ffi-obj "tcgetpgrp" libc (_fun _int -> _int)))



(define getpid (get-ffi-obj "getpid" libc (_fun -> _int)))
(define setpgid (get-ffi-obj "setpgid" libc (_fun _int _int -> _int)))

(define c-exit (get-ffi-obj "exit" libc (_fun _int -> _void)))
(define signal (get-ffi-obj "signal" libc (_fun _int (_fun _int -> _void) -> _void)))

(define (sigttou a) (void))

(signal 22 sigttou)

(require "terminal.rkt")
(require "termios.rkt")

(define job%
    (class object%
        (super-new)
        (define termios (new termios%))

        (define can-continue? #f)
        (define pgid 0)
        (define lock (make-semaphore 1))

        (define/public (get-semaphore) lock)

        (define/public (become-foreground-process terminal)
            (when can-continue?
                (send termios restore-tmodes terminal))

            (set-foreground-process-group terminal pgid))

        (define/public (stop terminal)
            (send termios save-tmodes terminal)
            (set! can-continue? #t))

        (define/public (get-pgid) pgid)
        (define/public (set-pgid id)
            (set! pgid id))
    ))
(require "shell.rkt")
(define shell (new shell%))

(define (set-job-pgid job pgid)
    (call-with-semaphore (send job get-semaphore)
        (lambda ()
            (when (eq? 0 (send job get-pgid))
                (send job set-pgid pgid)    
                (setpgid pgid pgid)))))

(define (launch-process job)
    (define pid (getpid))

    (set-job-pgid job pid)

    (send job become-foreground-process (send shell get-terminal))
    (execvp "python" '("python" #f))
    (c-exit 1))

(define (put-job-in-foreground job pid)
    (set-job-pgid job pid)

    (define terminal (send shell get-terminal))
    (send job become-foreground-process terminal)
    (wait #f)
    (send job stop terminal)
    (send shell become-foreground-process)
    
    (void))


(define (launch-job job)
    (define pid (fork))
     (if (eq? pid 0)
       (launch-process job)
       (put-job-in-foreground job pid)))

(define job (new job%))    

(launch-job job)