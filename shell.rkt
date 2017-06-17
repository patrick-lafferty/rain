#lang racket/base

;This module defines the namespace to be used for
;the shell's REPL

;(require racket/rerequire)
(require racket/class)
(require racket/list)
(require racket/match)
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
        (define termios (new termios% [is-shell #t]))

        (define terminal 0)
        (define shell-is-interactive (isatty terminal))

        (define pgid (getpid))

        (setpgid pgid pgid)
        (send termios save-tmodes terminal)
        (become-foreground-process)        

        (define/public (get-terminal)
            terminal)

        (define/public (get-pgid)
            pgid)
        
        (define/public (become-foreground-process)
            (set-foreground-process-group terminal pgid)
            (send termios restore-tmodes terminal))))

(require "profile.rkt")    
(setup-profile)

(require "builtins.rkt")

#|(define (combine-namespaces)
    ;(dynamic-rerequire "builtins.rkt")
    (let ([n (make-base-emptynamespace)]); (module->namespace "builtins.rkt")])
        (parameterize ([current-namespace n])
            (namespace-require "filesystem.rkt)
            (when (file-exists? user-profile)
                (namespace-require user-profile)))
            
        n))
        |#
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define shell-namespace ns) ;(combine-namespaces))

(require "jobs.rkt")

(define master-termios (new termios% [is-shell #f]))
(send master-termios save-tmodes 0)

(define shell (new shell%))
(define launcher (new launcher%))
(send launcher set-shell shell)

(define (unknown e code) 
    (let ([id (exn:fail:contract:variable-id e)])
        (when (eq? (first code) id)
            (printf "~a is unknown~n" id))))

;(require "builtins.rkt")
(define (run name [args '()] #:redirect-in [in ""] #:redirect-out [out ""] #:redirect-err [err ""])
    (list (list name args) in out err))
(define (pipe . jobs)
    (let ([jjobs (map (lambda (j) 
        (new job% [args (flatten (list (first j) #f))]
                [redirects (rest j)]
            )) jobs)])
        (send launcher launch-group jjobs)))

(define (exec code)
    (with-handlers 
        (
            ;[exn:fail:contract:variable? (lambda (e) (unknown e code))]
            [exn:fail? (lambda (e) (displayln e))])
        (let ([transformed-code code ])
            (let ([result (eval transformed-code shell-namespace)])
                (cond
                    [(void? result) (void)]
                    [else (displayln result)])))))

(define (handle-symbol s)
    (match s
;        ['reload (reload-shell)]
        ['exit (quit)]
        ['fg (send launcher fg)]
        [ _ (parameterize ([current-namespace shell-namespace])
                (if (is-in-namespace? s shell-namespace)
                    (displayln (eval s))    
                    (printf "~a is undefined ~n" s)))]
    ))

#|(define (reload-shell)
    (set! shell-namespace (combine-namespaces)))
|#
(define (quit)
    (send master-termios quit 0)
    (exit))

(provide exec #|reload-shell|# pipe handle-symbol quit shell% #|is-in-namespace?|# shell-namespace)