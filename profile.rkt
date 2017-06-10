#lang racket

(provide (all-defined-out))

(define user-profile (expand-user-path "~/.lush_profile"))
(define default-profile-contents 
    '(
        "#lang racket"
        "(provide (all-defined-out))"
    ))

(define (setup-profile)

    (unless (file-exists? user-profile)
        (let ([profile (open-output-file user-profile)])
            (for ([line default-profile-contents])
                (displayln line profile))    
            (close-output-port profile))))

(define (source filename)
    (if (file-exists? filename)    
        (let ([input-file (open-input-file filename)])
            (let ([code (read input-file)])
                (eval code)))
        (printf "Can't source ~a, file does not exist~n" filename)))
