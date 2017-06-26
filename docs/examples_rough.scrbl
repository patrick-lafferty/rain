(define (search-files names)
    (let ([names (map (lambda (x) (string-append x ".rkt")) names)])
        {ls names}))

{ls (map (lambda (x) (string-append x ".rkt")) '("shell" "builtins"))}