#lang racket

(define (can-execute path)
    (let ([filename 
            (cond 
                [(symbol? path) (symbol->string path)]
                [(string? path) path]
            )])
        (or (find-executable-path filename)
            (find-executable-path (format "~a.exe" filename)))))

(provide can-execute)