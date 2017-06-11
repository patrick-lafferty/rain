#lang racket

(require ffi/unsafe)
(require ffi/unsafe/define)

(define-ffi-definer define-libc (ffi-lib #f))
(define-ffi-definer define-libsignals (ffi-lib "libsignals" '(#f)))

(define-libsignals childStopped (_fun _int -> _int))
(define-libsignals childExited (_fun _int -> _int))

(define-libc fork (_fun -> _int))
(define-libc execvp (_fun _path (_list i _string) -> _int))
(define-libc wait (_fun _pointer -> _void))
(define-libc waitpid (_fun _int (status : (_ptr o _int)) _int 
    -> (r : _int) 
    -> (values status r)))
(define-libc kill (_fun _int _int -> _int))

(define-libc getpid (_fun -> _int))
(define-libc setpgid (_fun _int _int -> _int))

(define-libc exit (_fun _int -> _void))
(define-libc signal (_fun _int (_fun _int -> _void) -> _void))
(define-libc pipe (_fun (fd : (_ptr o (_array/list _int 2))) 
        -> (r : _int)
        -> (values fd r)))
(define-libc dup2 (_fun _int _int -> _void))
(define-libc read (_fun _int _pointer _int -> _int))
(define-libc close (_fun _int -> _int))

(require "terminal.rkt")
(require "termios.rkt")

(define SIGCONT 18)

(define job%
    (class object%
        (init args)
        (super-new)
        (define termios (new termios% [is-shell #f]))

        (define can-continue? #f)
        (define pid 0)
        (define pgid 0)
        (define argv args)

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

        (define/public (get-argv) argv)
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
        (define shell #f)
        (define/public (set-shell s) (set! shell s))
        (define stoppedJobs '())

        (define/public (fg)
            (match stoppedJobs
                [(cons head tail) 
                    (set! stoppedJobs tail)
                    (put-job-in-foreground head (send head get-pid))]
                [_ (displayln "no jobs")]))

        (define/public (launch-process job fd)
            (define pid (getpid))

            (set-job-pgid job pid)
            (when fd
                (let ([in (list-ref fd 0)]
                      [out (list-ref fd 1)])
                    (when in
                        (dup2 in 0))

                    (when out
                        (dup2 out 1))
                ))
                #|(define stdout-fd (list-ref fd 1))         
                (define stdin-fd (list-ref fd 0))  
                (dup2 stdout-fd 1)
                (close stdin-fd))
                |#

            (send job become-foreground-process (send shell get-terminal))
            (let ([argv (send job get-argv)])
                (execvp (first argv) argv))

            ;(when fd (close (list-ref fd 1)))

            (send job stop (send shell get-terminal))
            (exit 1))

        (define/public (put-job-in-foreground job pid)
            (set-job-pgid job pid)
            (send job set-pid pid)

            (define terminal (send shell get-terminal))
            (send job become-foreground-process terminal)
            
            (define-values (status result) (waitpid WAIT_ANY WUNTRACED))
            
            (cond
                [(= 1 (childStopped status)) (set! stoppedJobs (cons job stoppedJobs))]
                [(= 1 (childExited status)) void])
            (send job stop terminal)
            (send shell become-foreground-process)
            
            (void))

        (define/public (launch-job job is-top-level)

            (if is-top-level
                (begin
                    (let ([pid (fork)])
                        (if (eq? pid 0)
                            (launch-process job #f)
                            (put-job-in-foreground job pid))))
                (begin
                    (let-values ([(fd result) (pipe)])
                        (let ([stdout-fd (list-ref fd 1)]         
                              [stdin-fd (list-ref fd 0)]
                              [pid (fork)])         

                            (if (eq? pid 0)
                                (launch-process job fd)
                                (put-job-in-foreground job pid))

                            (close stdout-fd)
                            stdin-fd)))))
                            
        (define/public (run-job job fd-in fd-out)
            (let ([pid (fork)])
                (if (eq? pid 0)
                    (launch-process job (list fd-in fd-out))
                    (begin 
                        (put-job-in-foreground job pid)
                        (when fd-in (close fd-in))
                        (when fd-out (close fd-out) )
                    ))
            ))

        (define/public (launch-group jobs) 
            (let ([pipes 
                (map 
                    (lambda (x) (let-values ([(fd result) (pipe)]) fd)) 
                    (take jobs (sub1 (length jobs)))) ])
                (letrec ([helper 
                    (lambda (prev current fd-in fd-out)
                        (match current
                            [(cons a '()) 
                                (run-job a fd-in #f)]
                            [(cons a b) 
                                (let* ([fd (first fd-out)]
                                        [in (first fd)]
                                        [out (second fd)])
                                    (run-job a fd-in out)
                                    (helper a b in (rest fd-out)))]
                         ))])
                                    
                    (helper '() jobs #f pipes))))
        
        ))


(define launcher (new launcher%))

(provide launcher job%)