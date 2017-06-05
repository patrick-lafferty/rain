#lang racket

(define (can-execute file)
    (let ([filename 
            (cond 
                [(symbol? file) (symbol->string file)]
                [(string? file) file]
            )])
        (or (find-executable-path filename)
            (find-executable-path (format "~a.exe" filename)))))

(provide can-execute)