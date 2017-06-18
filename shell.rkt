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

;libc imports
(require ffi/unsafe)
(define libc (ffi-lib #f))
(define isatty (get-ffi-obj "isatty" libc (_fun _int -> _int)))
(define getpid (get-ffi-obj "getpid" libc (_fun -> _int)))
(define setpgid (get-ffi-obj "setpgid" libc (_fun _int _int -> _int)))

#|
(hash-set! sexp-table (quote id) 
                    (list 
                        'define
                        (flatten 
                            (list 
                                (quote id) 
                                (quote params))) 
                        (quote (flatten (map (lambda (b) (quote b)) body ...))))) 
|#

;(define (get x) (namespace-variable-value x))

(define (lookup x env) 
    (printf "lookup x: ~v env: ~v~n" x env)
    (printf "symbol? x: ~v~n" (symbol? x))
    (cond
        [(null? env) (namespace-variable-value x #t #f shell-namespace)]
        [else 
            (if (symbol? x)
                (let ([current (first env)])
                    (if (hash-has-key? current x)
                        (hash-ref current x)
                        (lookup x (rest env))))
                x)]))

(define (make-env params args)
    (let ([pairs (map cons params args)])
        (make-immutable-hash pairs)))

(define (interpret code [env (list sexp-table)])
    (printf "interpreting code: ~v~n" code)
    (match code
        ['() code]
        [(list 'define (cons id params) expr)
            (hash-set! (first env) id (list params expr))]
        [(list (? symbol? a) b ...) 
            (printf "a: ~v~nb: ~a~n" a b)
            (let ([proc (interpret a env)]) ;(apply (interpret a) b)])
                (if (list? proc)
                    (let ([params (first proc)]
                          [body (flatten (rest proc))])
                          (printf "params: ~v~nbody: ~v~n" params body)
                          (let ([arguments (make-env params (interpret b env))])
                            (printf "arguments: ~v~n" arguments)
                            (interpret body (cons arguments env))))

                    (begin 
                        (printf "applying proc: ~v with b: ~v~n" proc b)
                        (let ([args (map (lambda (x) (interpret x env)) b)])
                            (printf "args: ~v~n" args)
                            (apply proc args)))))]      

        [a (lookup a env)]
            ;(if (hash-has-key? sexp-table a)
            ;    (hash-ref sexp-table a)
            ;    (namespace-variable-value a))]
        [_ (printf "shouldn't get here: ~v~n" code)]))

(define (source) 
    (let ([input-file (open-input-file user-profile)])
        (let ([code (read input-file)])
            (interpret code)
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

(define master-termios (new termios% [is-shell #f]))
(send master-termios save-tmodes 0)

(define shell (new shell%))
(define launcher (new launcher%))
(send launcher set-shell shell)

#|
run is only used by sh-lang
sh-lang transforms {ls ...} into (run "ls" ...) which collects all the args and redirects
|#
(define (run name [args '()] #:redirect-in [in ""] #:redirect-out [out ""] #:redirect-err [err ""])
    (list (list name args) in out err))

#|
pipe is only used by sh-lang
sh-lang transforms {... | ... | ...} into (pipe (run ...) (run ...) (run ...))
pipe then creates all the necessary jobs and tells the launcher to run them
|#
(define (pipe . runs)
    (let ([jobs (map (lambda (j) 
        (new job% [args (flatten (list (first j) #f))]
                [redirects (rest j)]
            )) runs)])
        (send launcher launch-group jobs)))

;TODO: replace with interpreter
(define (exec code)
    (with-handlers 
        (
            [exn:fail? (lambda (e) (displayln e))])
        (let ([transformed-code code ])
            (let ([result (interpret code)]);(eval transformed-code shell-namespace)])
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
        [ _ (parameterize ([current-namespace shell-namespace])
                (if (is-in-namespace? s shell-namespace)
                    (displayln (eval s))    
                    (printf "~a is undefined ~n" s)))]
    ))

(require (only-in racket/base (define racket-define)))

(define sexp-table (make-hash))

#|(define-syntax define
    (syntax-rules ()
        [(_ (id . params) body ...) 
            (begin
                (hash-set! sexp-table (quote id) 
                    (list 
                        'define
                        (flatten 
                            (list 
                                (quote id) 
                                (quote params))) 
                        (quote (flatten (map (lambda (b) (quote b)) body ...))))) 
                (racket-define (id . params) body ...))]
        [(_ other ...) (racket-define other ...)]
                     
    ))|#

(define (export-to-file sexp-id filename) 
    (if (hash-has-key? sexp-table sexp-id)
        (let ([output (open-output-file filename #:exists 'append)])
            (write (hash-ref sexp-table sexp-id) output)
            (close-output-port output))
        (printf "Can't export ~a, it's not defined" sexp-id)))

(define (export-to-profile sexp-id)
    (export-to-file sexp-id user-profile))

(define (print-sexp sexp-id)
    (when (hash-has-key? sexp-table sexp-id)
        (displayln (hash-ref sexp-table sexp-id))))

(source)