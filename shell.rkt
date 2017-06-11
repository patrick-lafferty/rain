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

(require "profile.rkt")    
(setup-profile)

(define (combine-namespaces)
    (dynamic-rerequire "builtins.rkt")
    (let ([n (module->namespace "builtins.rkt")])
        (parameterize ([current-namespace n])
            (namespace-require "filesystem.rkt")
            (when (file-exists? user-profile)
                (namespace-require user-profile)))
            
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
    ;(writeln code)
    ;instead of this soup,
    ;map the code,
    ; 
    (define (getnum)
        (define x -1)
        (lambda () 
            (set! x (add1 x))
            x))

    (define (change lst)
        (define getn (getnum))
        (define top-level is-top-level)
        (set! is-top-level #f)
        (map (lambda (a) 
            (if (= (getn) 0)
                (begin 
                ;(printf "getn0 ~a" a)
                (match a
                    ;[(list 'define a b ...) (list 'define a (change b))]
                    ['if a]
                    ['set a]
                    [#t a]
                    [#f a]
                    [_ (cond
                    ;todo: ignore checking first elem if in define/set/let
                            [(list? a) (change a)]
                            [(regexp? a) a]
                            [(char? a) a]
                            [(is-in-namespace? a shell-namespace) a]
                            [(is-in-namespace? (quote a) shell-namespace) a]
                            ;[(can-execute a) 
                            ;    (if top-level
                            ;        (launch (format "~a" a))
                            ;        (evaluate (format "~a" a)))]
                            [else a]
                        )] 
                ))
                (if (list? a)
                    (change a)
                    a)
        )) lst))

    (define (rec c acc)
        (match c
            ['() acc]
            [(cons first rest)
                (if (list? first)
                    (begin
                        (if (eqv? (list-ref first 0) 'quote)
                            (rec rest (cons first acc))
                            (let ([result (reverse (rec first '()))])
                                (rec rest (cons result acc)))))
                    (begin   
                        (cond
                        ;[(eqv? first 'quote) (rec rest ]
                        [(eqv? first 'define) (displayln "display~")]
                        [(is-in-namespace? first shell-namespace)
                            ;(parameterize ([current-namespace shell-namespace]) 
                            ;(printf "f: ~a is: ~a isq: ~a~n" first (is-in-namespace? first) (is-in-namespace? (quote first)))
                            ;#f)
                          ;  (or
                                ;(is-in-namespace? first) 
                           ;     (is-in-namespace? (quote first))))   
                            (rec rest (cons first acc))]
                        [(can-execute first)
                            ;(printf "can-execute: ~a~n" first)
                            (let ([stringified (format "~a" first)])  
                                (if is-top-level
                                    (begin 
                                        (set! is-top-level #f)
                                        (rec rest (append acc `(,stringified launch))))
                                    (rec rest (append acc `(,stringified evaluate)))))]
                        [else (rec rest (cons first acc))])))]
            [_ (cons c acc)]))

    ;(rec code '()))
    ;(change code))
    code)


;(define-syntax-rule (app @ ( f . args))
;    (printf "@f: ~a args: ~a" f args))
;(define-syntax @ 
;    (syntax-rules ()
;        [(@ f . args) (printf "@f: ~a args: ~a" f args)]))    
   ;(if (identifier-binding f) 
    ;(f args)
    ;(printf "~a is unbound~n" f)
    ;))



(define (exec code)
    (with-handlers 
        (
            ;[exn:fail:contract:variable? (lambda (e) (unknown e code))]
            [exn:fail? (lambda (e) (displayln e))])
        (let ([transformed-code (transform code)]);(reverse (transform code))])
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
        [ _ (parameterize ([current-namespace shell-namespace])
                (if (is-in-namespace? s shell-namespace)
                    (displayln (eval s))    
                    (printf "~a is undefined ~n" s)))]
    ))

(define (reload-shell)
    (set! shell-namespace (combine-namespaces)))

(define (quit)
    (send master-termios quit 0)
    (exit))

(provide exec reload-shell handle-symbol quit shell%)