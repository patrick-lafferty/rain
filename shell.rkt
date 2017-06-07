#lang racket

;This module defines the namespace to be used for
;the shell's REPL

(require racket/rerequire)
(require "filesystem.rkt")
(require "terminal.rkt")
(require "termios.rkt")

;libc imports
(require ffi/unsafe)
(define libc (ffi-lib #f))
(define isatty (get-ffi-obj "isatty" libc (_fun _int -> _int)))
(define getpid (get-ffi-obj "getpid" libc (_fun -> _int)))
(define setpgid (get-ffi-obj "setpgid" libc (_fun _int _int -> _int)))

(define shell% 
    (class object%
        
        (super-new)
        (define termios (new termios%))

        (define terminal 0)
        (define shell-is-interactive (isatty terminal))

        (define pgid (getpid))

        (setpgid pgid pgid)
        (send termios save-tmodes terminal)
        (become-foreground-process)        
        ;(send termios save-tmodes terminal)

        (define/public (get-terminal)
            terminal)

        (define/public (get-pgid)
            pgid)
        
        (define/public (become-foreground-process)
            (set-foreground-process-group terminal pgid)
            (send termios restore-tmodes terminal))))

(define (combine-namespaces)
    (dynamic-rerequire "builtins.rkt")
    (let ([n (module->namespace "builtins.rkt")])
        (parameterize ([current-namespace n])
            (namespace-require "filesystem.rkt"))
        n))

(define shell-namespace (combine-namespaces))

(require "jobs.rkt")

(define shell (new shell%))
(define launcher (new launcher% [current-shell shell]))

(define (start-job id)
    (define job (new job% [args (list id #f)]))    

    (send launcher launch-job job))

(define (unknown e code) 
    (let ([id (exn:fail:contract:variable-id e)])
        (when (eq? (first code) id)
            (if (can-execute id)
                (start-job (format "~a" id))
                (printf "~a is unknown~n" id)))))


(define (exec code) 
    (with-handlers 
        (
            [exn:fail:contract:variable? (lambda (e) (unknown e code))]
            [exn:fail? (lambda (e) (void))])
        (let ([result (eval code shell-namespace)])
            (cond
                [(void? result) (void)]
                [else (displayln result)]))))

(define (handle-symbol s)
    (match s
        ['reload (reload-shell)]
        ['exit (exit)]
        ['fg (send launcher fg)]
        [ _ (printf "~a is undefined ~n" s)]
    ))

(define (reload-shell)
    (set! shell-namespace (combine-namespaces)))

(provide exec reload-shell handle-symbol shell%)