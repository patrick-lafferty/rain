#lang racket

;This module defines built-in functions to be used
;in the shell's REPL

#|
(require racket/bool)
(require racket/bytes)
(require racket/class)
(require racket/cmdline)
(require racket/contract)
(require racket/dict)
(require racket/file)
(require racket/format)
(require racket/function)
(require racket/future)
(require racket/include)
(require racket/list)
(require racket/local)
(require racket/logging)
(require racket/match)
(require racket/math)
(require racket/path)
(require racket/place)
(require racket/port)
(require racket/pretty)
(require racket/promise)
(require racket/sequence)
(require racket/set)
(require racket/shared)
(require racket/stream)
(require racket/string)
(require racket/system)
(require racket/tcp)
(require racket/udp)
(require racket/unit)
(require racket/vector)
|#

(require "version.rkt")
    
(define (is-in-namespace? symbol [namespace (current-namespace)])
    (if (member symbol (namespace-mapped-symbols namespace))
        #t
        #f))

(require "jobs.rkt")

(define (expand path) 
    (let ([exp (pregexp (string-replace path "*" "\\W*"))])
;(for ([p (in-directory "/example")]))
        (let ([expanded (filter (lambda (dir) (regexp-match? exp dir)) (directory-list))])
            (displayln expanded))))
    ;(regexp-match? #px"\\W*.txt" "text.txt")
            

(define (run name [args '()] #:redirect-out [out ""] #:redirect-in [in ""] #:redirect-err [err ""])
    (list name args))

(define (pipe . jobs)
    (let ([jjobs (map (lambda (j) 
        (new job% [args (flatten (list j #f))])) jobs)])
        (send launcher launch-group jjobs)))

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
                        (quote (flatten (map (lambda (b) (quote b)) body ...))))) 
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

(provide is-in-namespace?)
        