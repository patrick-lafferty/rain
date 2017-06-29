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
#lang racket

;the module implements job control (a job is a shell-launched process)

(provide 
 ;represents a single process
 job%

 ;starts single or groups of jobs and tracks stopped jobs
 launcher%
 )

(require ffi/unsafe)
(require ffi/unsafe/define)

(define-ffi-definer define-libc (ffi-lib #f))
(define-ffi-definer define-libexplain (ffi-lib "libexplain" '(#f)))
(define-ffi-definer define-libsignals (ffi-lib "libsignals" '(#f)))

(define-libsignals childStopped (_fun _int -> _int))
(define-libsignals childExited (_fun _int -> _int))

(define-libc fork (_fun -> _int))
(define-libc execvp (_fun _path (_list i _string) -> _int))
(define-libc wait (_fun _pointer -> _void))
(define-libc waitpid (_fun _int (status : (_ptr o _int)) _int 
    -> (r : _int) 
    -> (values status r)))
(define-libexplain explain_waitpid_or_die (_fun _int (status : (_ptr o _int)) _int 
    -> (r : _int) 
    -> (values status r)))
(define-libexplain explain_waitpid (_fun _int (_ptr i _int) _int 
    -> _path))
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
(define-libc open (_fun _path _int _int -> _int))
(define-libc close (_fun _int -> _int))

(require "terminal.rkt")
(require "termios.rkt")

(define SIGCONT 18)

(define O_RDONLY 0)
(define O_WRONLY 1)
(define O_RDWR 2)
(define O_CREAT 64)
(define S_IWUSR 128)
(define S_IRUSR 256)

(define job%
    (class object%
        (init args) ;redirects)
        (super-new)
        (define termios (new termios% [is-shell #f]))

        (define can-continue? #f)
        (define pid 0)
        (define pgid 0)
        (define argv args)
        (define stdin "") ; (list-ref redirects 0))
        (define stdout "") ;(list-ref redirects 1))
        (define stderr "") ;(list-ref redirects 2))

        (define/public (get-stdin) stdin)
        (define/public (get-stdout) stdout)
        (define/public (get-stderr) stderr)

        (define/public (set-pipes in out err)
            (redirect-in in)
            (redirect-out out)
            (redirect-err err))

        (define sub-process #f)

        (define/public (set-proc proc) (set! sub-process proc))
        (define/public (get-proc) sub-process)

        (define/public (redirect-in in) (set! stdin in))
        (define/public (redirect-out out) (set! stdout out))
        (define/public (redirect-err err) (set! stderr err))

        (define/public (become-foreground-process terminal)
            (when can-continue?
                (displayln "[bfp] can-continue")
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

(define (set-job-pgid job pid pgid)
    (when (= 0 pgid);(send job get-pgid))
        (send job set-pgid pid) 
        (setpgid pid pid)))

(define WAIT_ANY -1)
(define WUNTRACED 2)

;(define is-in-thread? (make-parameter #f))
(require "job-parameters.rkt")

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
                    (put-job-in-foreground head (send head get-pid) (send head get-pgid))]
                [_ (displayln "no jobs")]))

        (define/public (launch-process job fd)
            (printf "[launch-process] start")
            (define pid (getpid))
            (set-job-pgid job pid)

            (when fd
                (let ([in (list-ref fd 0)]
                      [out (list-ref fd 1)])
                    (when in
                        (dup2 in 0)
                        (close in))

                    (when out
                        (dup2 out 1)
                        (close out))
                ))

            (let ([stdin (send job get-stdin)]
                  [stdout (send job get-stdout)]
                  [stderr (send job get-stderr)])
                  
                  (when (non-empty-string? stdin)
                    (let ([in (open stdin O_RDONLY 0)])
                        (dup2 in 0)
                        (close in)))

                  (when (non-empty-string? stdout)
                    (let ([out (open stdout (bitwise-ior O_WRONLY O_CREAT) (bitwise-ior S_IWUSR S_IRUSR))])
                        (dup2 out 1)
                        (close out)))
                
                  (when (non-empty-string? stderr)
                    (let ([err (open stderr (bitwise-ior O_WRONLY O_CREAT) (bitwise-ior S_IWUSR S_IRUSR))])
                        (dup2 err 2)
                        (close err)))
                  )
            
            (printf "[launch-process] about to execvp")
            (send job become-foreground-process (send shell get-terminal))
            (let ([argv (send job get-argv)])
                (let ([result (execvp (first argv) argv)])
                    (printf "execp result: ~v~n" result)))

            (send job stop (send shell get-terminal))
            (exit 1))

        (define/public (launch-subprocess job in out err pgid)
            
            (printf "[launch-process] about to subprocess~n")
            (let* ([argv (send job get-argv)]
                    [path (find-executable-path (first argv))]
                    [args (map (lambda (x) (if x x '())) (rest argv))]
                    [sp-args (flatten (list out in err path args))])

                (let-values ([(proc out in err) (apply subprocess sp-args)])
                    (set-job-pgid job (subprocess-pid proc) pgid)
                    (send job set-pid (subprocess-pid proc))
                    (unless (is-in-thread?)
                        (send job become-foreground-process (send shell get-terminal)))
                    (send job set-pipes in out err)
                    (send job set-proc proc))))

        (define/public (put-job-in-foreground job pid pgid)
            ;(set-job-pgid job pid pgid)
            ;(send job set-pid pid)

            (define terminal (send shell get-terminal))
            (send job become-foreground-process terminal)
            
            ;TODO: change waiting to wait on all from process group
            (printf "[pgif] waitpid id: ~v~n" (* -1 pgid))
            (define-values (status result) (waitpid -1 WUNTRACED)) ;WAIT_ANY WUNTRACED))
            ;(subprocess-wait (send job get-proc))
            ;(define status 1)
            ;(define result 1)
            ;(printf "[pgif] status: ~v result: ~v~n" status result)
            ;(printf "[pgif] explain: ~v~n" (explain_waitpid -1 status WUNTRACED))
            ;(displayln "done")
            (displayln "")

            (cond
                [(= 1 (childStopped status)) (set! stoppedJobs (cons job stoppedJobs))]
                [(= 1 (childExited status)) void])
            (send job stop terminal)
            (send shell become-foreground-process)
            
            (void))

        (define/public (run-job job fd-in fd-out)
            (printf "[run-job] job: ~v fd-in: ~v fd-out: ~v~n" job fd-in fd-out)
            (let ([pid (fork)])
                (printf "[run-job] after fork")
                (if (eq? pid 0)
                    (launch-process job (list fd-in fd-out))
                    (begin 
                        (printf "[run-job] parent")
                        (put-job-in-foreground job pid)
                        (printf "[run-job] parent after put-job-in-foreground")
                        (when fd-in (close fd-in))
                        (when fd-out (close fd-out))
                    ))
            ))

        (define/public (run-job-subprocess job in out err pgid)
            ;(let-values ([(proc stdout stdin stderr) (subprocess #f #f #f (find-executable-path ))]))
            (let ([proc (launch-subprocess job in out err pgid)])
                (printf "[rjs] is-in-thread? ~v~n" (is-in-thread?))
                (unless (is-in-thread?)
                    (put-job-in-foreground job (subprocess-pid (send job get-proc)) (send job get-pgid)))))

        (define/public (launch-group jobs)
            (subprocess-group-enabled #f)
;TODO: remember to close all opened ports
            (define stdin 
                (if (is-in-thread?)
                    #f
                    (let ([job-in (send (first jobs) get-stdin)])
                        (if (non-empty-string? job-in)
                            (open-input-file job-in)
                            (current-input-port)))))
            
            (define stdout
                (if (is-in-thread?)
                    #f
                    (let ([job-out (send (last jobs) get-stdout)])
                        (if (non-empty-string? job-out)
                            (open-output-file job-out)
                            (current-output-port)))))

            (define (helper prev current in out err first? pgid)
                (subprocess-group-enabled first?) 

                (match current
                    [(cons a '())
                        (run-job-subprocess a in stdout err pgid)]
                    [(cons a b)
                            (run-job-subprocess a in out err pgid)
                            (let ([next-in (send a get-stdout)]
                                    [next-err (send a get-stderr)]
                                    [pgid (send a get-pgid)])
                                (helper a b next-in #f #f #f pgid))]))

            (helper '() jobs stdin stdout #f #t 0))
            ;(if (= 1 (length jobs))
            ;    (helper '() jobs stdin (current-output-port) (current-error-port) #t 0)
            ;    (helper '() jobs stdin #f #f #t 0)))


        (define/public (launch-group-old jobs) 
            (printf "[launch-group] jobs: ~v~n" jobs)
            (let ([pipes 
                (map 
                    (lambda (x) (let-values ([(fd result) (pipe)]) fd)) 
                    (take jobs (sub1 (length jobs)))) ])
                (letrec ([helper 
                    (lambda (prev current fd-in fd-out)
                        (match current
                            [(cons a '()) 
                                (printf "[launch-group] cons a: ~v~n" a)
                                (run-job a fd-in #f)]
                            [(cons a b) 
                                (printf "[launch-group] cons a: ~v b: ~v~n" a b)
                                (let* ([fd (first fd-out)]
                                        [in (first fd)]
                                        [out (second fd)])
                                    (run-job a fd-in out)
                                    (helper a b in (rest fd-out)))]
                         ))])
                                    
                    (helper '() jobs #f pipes))))
        ))
