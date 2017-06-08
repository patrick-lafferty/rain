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

(define (combine-namespaces)
    (dynamic-rerequire "builtins.rkt")
    (let ([n (module->namespace "builtins.rkt")])
        (parameterize ([current-namespace n])
            (namespace-require "filesystem.rkt"))
        n))

(define shell-namespace (combine-namespaces))

(require "jobs.rkt")

(define master-termios (new termios% [is-shell #f]))
(send master-termios save-tmodes 0)

(define shell (new shell%))
(send launcher set-shell shell)

(define (unknown e code) 
    (let ([id (exn:fail:contract:variable-id e)])
        (when (eq? (first code) id)
            (printf "~a is unknown~n" id))))

(define (print-tabs t)
    (when (> t 0)
        (display " ")
        (print-tabs (sub1 t))))

(require "builtins.rkt")

(define (exec2 code) 
    (define (rec c tab)
        (match c
            [(cons a b)
                (print-tabs tab)
                (printf "(~a~n" (is-in-namespace? a))
                (rec b (add1 tab))]
            [_ (print-tabs tab) (printf "~a)~n" (is-in-namespace? c))]))
    (rec code 0))

(define (transform code)
    (define is-top-level #t)
    (define (rec c acc)
        (match c
            ['() acc]
            [(cons first rest)
                (if (list? first)
                    (begin
                        (let ([result (reverse (rec first '()))])
                            (rec rest (cons result acc))))
                    (begin     
                        (cond
                         [(is-in-namespace? first)
                            (rec rest (cons first acc))]
                         [(can-execute first)
                            (let ([stringified (format "~a" first)])  
                                (if is-top-level
                                    (begin 
                                        (set! is-top-level #f)
                                        (rec rest (append acc `(,stringified launch))))
                                    (rec rest (append acc `(,stringified evaluate)))))]
                         [else (rec rest (cons first acc))])))]
            [_ (cons c acc)]))

    (rec code '()))

(define (exec code)
    (with-handlers 
        (
            [exn:fail:contract:variable? (lambda (e) (unknown e code))]
            [exn:fail? (lambda (e) (displayln e))])
        (let ([transformed-code (reverse (transform code))])
            ;(writeln transformed-code)
            (let ([result (eval transformed-code shell-namespace)])
                (cond
                    [(void? result) (void)]
                    [else (displayln result)])))))

(define (handle-symbol s)
    (match s
        ['reload (reload-shell)]
        ['exit (quit)]
        ['fg (send launcher fg)]
        [ _ (printf "~a is undefined ~n" s)]
    ))

(define (reload-shell)
    (set! shell-namespace (combine-namespaces)))

(define (quit)
    (send master-termios quit 0)
    (exit))

(provide exec reload-shell handle-symbol shell%)