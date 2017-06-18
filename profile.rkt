#lang racket/base

(require racket/file)

(provide (all-defined-out))

(define user-profile (expand-user-path "~/.lush_profile"))
(define default-profile-contents 
    '(
        ;"#lang racket/base"
        "(provide (all-defined-out))"
    ))

(define (setup-profile shell-namespace)

    (unless (file-exists? user-profile)
        (let ([profile (open-output-file user-profile)])
            (for ([line default-profile-contents])
                (displayln line profile))    
            (close-output-port profile))))
            
    ;(source user-profile shell-namespace))

(require racket/rerequire)

(define (source filename shell-namespace)
    (if (file-exists? filename)   
        (parameterize ([current-namespace shell-namespace]) 
        (dynamic-require-for-syntax filename 0))
        #|(let ([input-file (open-input-file filename)])
            (let ([code (read input-file)])
                (eval code shell-namespace)))|#
        ;(load filename)
        (printf "Can't source ~a, file does not exist~n" filename)))
