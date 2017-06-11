#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

(require "version.rkt")
    
(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace)); (current-namespace)))
        #t
        #f))

(require "jobs.rkt")

(define (expand path) 
    (let ([exp (pregexp (string-replace path "*" "\\W*"))])
;(for ([p (in-directory "/example")]))
        (let ([expanded (filter (lambda (dir) (regexp-match? exp dir)) (directory-list))])
            (displayln expanded))))
    ;(regexp-match? #px"\\W*.txt" "text.txt")


            

;top-level interactive programs that print directly to stdout
(define (launch2 path . arguments) 
    ;(printf "launch ~a ~a~n" path arguments)
    (let* ([flattened-args (flatten (list path arguments #f))]
           [job (new job% [args flattened-args])])   
        ;(printf "flattened args: ~a ~n" flattened-args)
        (send launcher launch-job job #t)))

;child jobs that redirect stdout to a separate buffer
;TODO: need to close fds 
(define (evaluate2 path . arguments) 
    (define job (new job% [args (flatten (list path arguments #f))]))    
    (format "/dev/fd/~a" (send launcher launch-job job #f)))

(define (launch path)
    (lambda arguments
        (let* ([flattened-args (flatten (list path arguments #f))]
           [job (new job% [args flattened-args])])   
            (send launcher launch-job job #t))))

(define (evaluate path)
    (lambda arguments
        (let ([job (new job% [args (flatten (list path arguments #f))])])    
            (format "/dev/fd/~a" (send launcher launch-job job #f)))))

(define (run name [args '()] #:redirect-out [out ""] #:redirect-in [in ""] #:redirect-err [err ""])
    (list name args))
    ;(format "running ~a" args))
;    (foldl (lambda (x acc)
;        (if (null? acc)
;            )) args)

(define (pipe . jobs)
    (let ([jjobs (map (lambda (j) 
        (new job% [args (flatten (list j #f))])) jobs)])
        (send launcher launch-group jjobs)))
#|
ls | sort | less
   ^      ^
  fd1    fd2

(ls #f (first fd1))
(sort (second fd1) fd2)
(less fd2 #f)

ls | sort

pipes: '(fd1)

(ls)

need to connect ls stdout to sort stdin
need to connect sort stdout to less stdin

|#
    

(require (only-in racket/base (define racket-define)))

(define sexp-table (make-hash))

(define-syntax define
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
                        (quote (flatten (map (lambda (b) (quote b)) body ...))))) ;quote body ...)))
                ;(racket-define id (lambda params body ... )))];(id . params) body ...)) ]
                (racket-define (id . params) body ...))]
        [(_ other ...) (racket-define other ...)]
                     
    ))

(require "profile.rkt")

(define (export-to-file sexp-id filename) 
    (if (hash-has-key? sexp-table sexp-id)
        (let ([output (open-output-file filename #:exists 'append)])
            (write (hash-ref sexp-table sexp-id) output)
            (close-output-port output))
        (printf "Can't export ~a, it's not defined" sexp-id)))

(define (export-to-profile sexp-id)
    (export-to-file sexp-id user-profile))

;(define-syntax @ 
;    (syntax-rules ()
;        [(@ f . args) (printf "@f: ~a args: ~a~" f args)])) 
;(define-syntax-rule (@ lst) (@@ (quote lst)))
;(define-syntax @@ (syntax-rules() [(@@ lst) (printf "@@f: ~a~n" lst)]))

#|(provide (except-out (all-from-out racket)
                     #%app)
         (rename-out [app #%app]))
|#
#|(define-syntax-rule (sh-app f arg ...) 
                ;(if (string? f) 
                    (begin 
                        (printf "~v" f)
                        (flush-output)
                ;        (displayln "switching to shell mode")
                ;        f)
                    (apply f arg ...)))
            (provide (rename-out [sh-app #%app]))
|#
(provide is-in-namespace? launch evaluate); (rename-out [app #%app]))
        