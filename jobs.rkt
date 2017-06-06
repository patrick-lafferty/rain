#lang racket

(require ffi/unsafe)

(define libc (ffi-lib #f))

(define fork (get-ffi-obj "fork" libc (_fun -> _int)))
(define execvp (get-ffi-obj "execvp" libc (_fun _path (_list i _string) -> _int)))
(define wait (get-ffi-obj "wait" libc (_fun _pointer -> _void)))

(define getpgrp (get-ffi-obj "getpgrp" libc (_fun -> _int)))
(define tcgetpgrp (get-ffi-obj "tcgetpgrp" libc (_fun _int -> _int)))
(define tcsetpgrp (get-ffi-obj "tcsetpgrp" libc (_fun _int _int -> _int)))

(define-cstruct _termios 
    (
        [c_iflag _uint64]
        [c_oflag _uint64]
        [c_cflag _uint64]
        [c_lflag _uint64]
        [c_cc (_list i _byte)]
        [c_ispeed _int64]
        [c_ospeed _int64]
    ))

(define tcgetattr (get-ffi-obj "tcgetattr" libc (_fun _int _termios-pointer -> _int)))
(define tcsetattr (get-ffi-obj "tcsetattr" libc (_fun _int _int _termios-pointer -> _int)))

(define isatty (get-ffi-obj "isatty" libc (_fun _int -> _int)))
(define getpid (get-ffi-obj "getpid" libc (_fun -> _int)))
(define setpgid (get-ffi-obj "setpgid" libc (_fun _int _int -> _int)))

(define fdopen (get-ffi-obj "fdopen" libc (_fun _int _path -> _int)))

(define c-exit (get-ffi-obj "exit" libc (_fun _int -> _void)))

(define shell-terminal 0)
(define shell-is-interactive (isatty shell-terminal))

(define shell-pgid (getpid))
(tcsetpgrp shell-terminal shell-pgid)

(define shell-tmodes (make-termios 0 0 0 0 (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0 0))

(tcgetattr shell-terminal shell-tmodes)

(define signal (get-ffi-obj "signal" libc (_fun _int (_fun _int -> _void) -> _void)))

(define (sigttou a) (void))

(signal 22 sigttou)

(define (launch-process)
    (define pid (getpid))
    (define pgid pid)
    
    (setpgid pid pgid)
    (tcsetpgrp shell-terminal pgid)
    (execvp "python" '("python" #f))
    (c-exit 1))

(define (put-job-in-foreground pgid)
    (setpgid pgid pgid)
    (tcsetpgrp shell-terminal pgid)
    (wait #f)
    ;(displayln "im back")
    (tcsetpgrp shell-terminal shell-pgid)
    ;(tcgetattr shell-terminal job-modes)
    (tcsetattr shell-terminal 1 shell-tmodes)
)

(define (launch-job)
    (define pid (fork))
     (if (eq? pid 0)
       (launch-process)
       (put-job-in-foreground pid)))
    

(launch-job)
;(displayln "here")

;this works
;(define pid (fork))
;(if (eq? pid 0)
;    ((execvp "python" '("python" #f))
 ;       (exit))
;    (wait #f))
   

;(define o (process/ports (current-output-port) (current-input-port) (current-output-port) "python"))
;(define proc (list-ref o 4))
;(proc 'wait)

