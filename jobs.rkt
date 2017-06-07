#lang racket

(require ffi/unsafe)
(require ffi/unsafe/define)

(define libc (ffi-lib #f))

(define-ffi-definer define-libc (ffi-lib #f))
(define-ffi-definer define-libsignals (ffi-lib "libsignals" '(#f)))

(define-libsignals childStopped (_fun _int -> _int))
(define-libsignals childExited (_fun _int -> _int))

(define fork (get-ffi-obj "fork" libc (_fun -> _int)))
(define execvp (get-ffi-obj "execvp" libc (_fun _path (_list i _string) -> _int)))
(define wait (get-ffi-obj "wait" libc (_fun _pointer -> _void)))
(define-libc waitpid (_fun _int (status : (_ptr o _int)) _int 
    -> (r : _int) 
    -> (values status r)))
(define-libc kill (_fun _int _int -> _int))

(define getpid (get-ffi-obj "getpid" libc (_fun -> _int)))
(define setpgid (get-ffi-obj "setpgid" libc (_fun _int _int -> _int)))

(define c-exit (get-ffi-obj "exit" libc (_fun _int -> _void)))
(define signal (get-ffi-obj "signal" libc (_fun _int (_fun _int -> _void) -> _void)))

(require "terminal.rkt")
(require "termios.rkt")

(define SIGCONT 18)

(define job%
    (class object%
        (super-new)
        (define termios (new termios%))

        (define can-continue? #f)
        (define pid 0)
        (define pgid 0)

        (define/public (become-foreground-process terminal)
            (when can-continue?
                (kill pid SIGCONT)
                (send termios restore-tmodes terminal))

            (set-foreground-process-group terminal pgid))

        (define/public (stop terminal)
            (send termios save-tmodes terminal)
            (set! can-continue? #t))

        (define/public (get-pgid) 
            pgid)

        (define/public (set-pgid id)
            (set! pgid id))

        (define/public (get-pid) pid)

        (define/public (set-pid id)
            (set! pid id))
    ))

(define (set-job-pgid job pgid)
    (when (= 0 (send job get-pgid))
        (send job set-pgid pgid) 
        (setpgid pgid pgid)))

(define WAIT_ANY -1)
(define WUNTRACED 2)

(define launcher%
    (class object%
        (super-new)
        (init current-shell)
        (define shell current-shell)
        (define stoppedJobs '())

        (define/public (fg)
            (define job (first stoppedJobs))
            (put-job-in-foreground job (send job get-pid))
            )

        (define/public (launch-process job)
            (define pid (getpid))

            (set-job-pgid job pid)

            (send job become-foreground-process (send shell get-terminal))
            (execvp "vim" '("vim" #f))
            (send job stop (send shell get-terminal))
            (c-exit 1))

        (define/public (put-job-in-foreground job pid)
            (set-job-pgid job pid)
            (send job set-pid pid)

            (define terminal (send shell get-terminal))
            (send job become-foreground-process terminal)
            
            (define-values (status result) (waitpid WAIT_ANY WUNTRACED))
            
            (cond
                [(= 1 (childStopped status)) (set! stoppedJobs (list job))]
                [(= 1 (childExited status)) (void)])
            (send job stop terminal)
            (send shell become-foreground-process)
            
            (void))

        (define/public (launch-job job)
            (define pid (fork))
            (if (eq? pid 0)
                (launch-process job)
                (put-job-in-foreground job pid)))))

(provide launcher% job%)