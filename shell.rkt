#lang racket/base

;This module defines the namespace to be used for
;the shell's REPL

(provide 
 ;runs code in the shell's namespace
 exec 
 ;performs different actions based on the symbol
 handle-symbol 
)

(require racket/class)
(require racket/list)
(require racket/match)
(require "filesystem.rkt")
(require "terminal.rkt")
(require "termios.rkt")
(require "profile.rkt")    
(require "builtins.rkt")
(require "jobs.rkt")
(require "debug_printf.rkt")
(require "interpreter.rkt")
(require "env.rkt")

;libc imports
(require ffi/unsafe)
(define libc (ffi-lib #f))
(define isatty (get-ffi-obj "isatty" libc (_fun _int -> _int)))
(define getpid (get-ffi-obj "getpid" libc (_fun -> _int)))
(define setpgid (get-ffi-obj "setpgid" libc (_fun _int _int -> _int)))

(define (source) 
    (let ([input-file (open-input-file user-profile)])
        (letrec ([read-all (lambda _
            (let ([code (read input-file)])
                (unless (eof-object? code)
                    (writeln code)
                    (interpret code (list profile-env) #t)
                    (read-all))))])
            (read-all)
        )))


(define shell% 
    (class object%
        
        (super-new)
        (setup-profile shell-namespace)

        (define termios (new termios% [is-shell #t]))

        (define terminal 0)
        (define shell-is-interactive (isatty terminal))

        (define pgid (getpid))

        (setpgid pgid pgid)
        (send termios save-tmodes terminal)
        (become-foreground-process)        

        (define/public (get-terminal) terminal)

        (define/public (get-pgid) pgid)
        
        (define/public (become-foreground-process)
            (set-foreground-process-group terminal pgid)
            (send termios restore-tmodes terminal))))

(define-namespace-anchor shell-namespace-anchor)
(define shell-namespace (namespace-anchor->namespace shell-namespace-anchor))
(set-shell-namespace! shell-namespace)

(define master-termios (new termios% [is-shell #f]))
(send master-termios save-tmodes 0)

(define shell (new shell%))
(define launcher (new launcher%))
(send launcher set-shell shell)

#|
run is only used by sh-lang
sh-lang transforms {ls ...} into (run "ls" ...) which collects all the args and redirects
|#
(define (run name . args) 
    (debug-printf "running name: ~v args: ~v~n" name args)
    (list name args))


#|
pipe is only used by sh-lang
sh-lang transforms {... | ... | ...} into (pipe (run ...) (run ...) (run ...))
pipe then creates all the necessary jobs and tells the launcher to run them
|#
(define (pipe runs redirects)
    (debug-printf "pipe runs: ~v~nredirects: ~v~n" runs redirects)
    (let ([jobs (map (lambda (j) 
        (debug-printf "args: ~v~n" (flatten (list j #f)))
        (new job% [args (flatten (list j #f))]
            )) (filter (lambda (l) (not (null? l))) runs))])
        (debug-printf "jobs: ~v~n" jobs)
        (send (first jobs) redirect-in (first redirects))
        (send (last jobs) redirect-out (second redirects))
        (send (last jobs) redirect-err (third redirects))
        (send launcher launch-group jobs)))

(define (exec code)
    (with-handlers 
        ([exn:fail? (lambda (e) (displayln e))])
        (let ([transformed-code code ])
            (let ([result (interpret code (list repl-env profile-env) #t)])
                (cond
                    [(void? result) (void)]
                    [else (displayln result)])))))

#|
called when user input is a symbol not a list.
First checks to see if it is a builtin symbol,
and if not checks to see if its a value we can print
|#
(define (handle-symbol s)
    (match s
        ['exit 
            (send master-termios quit 0)
            (exit)]
        ['fg (send launcher fg)]
        [ _ (let ([value (lookup s (list repl-env profile-env))])
                (if value
                    (displayln value)
                    (printf "~a is undefined~n" s)))]
    ))



(source)