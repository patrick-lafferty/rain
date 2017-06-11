#lang racket

(define (sh-read-proc char in src ln col pos)
    (define epip '(#\e #\p #\i #\p))
    (define (transform lst)
        (let ([replaced (map (lambda (c) (if (char=? c #\|) epip c)) lst)])
            (flatten (list #\( (reverse (flatten replaced)) #\)))))    
    (define (collect-until delim acc)
        (let ([c (read-char in)])
            (if (char=? c delim) acc (collect-until delim (cons c acc)))))
    (let* ([lst (collect-until #\} '())]
            [str (list->string (transform lst))])
        
        (read (open-input-string str))))

(provide sh-read-proc) 