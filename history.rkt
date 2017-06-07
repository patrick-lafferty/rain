#lang racket

(define history%
    (class object%
        (super-new)
        (define past '())

        (define/public (add line)
            (set! past (cons line past)))
        
        (define/public (get i)
            (if (and 
                    (> i -1)
                    (< i (length past)))
                (list-ref past i)
                ""))

        (define/public (get-length) (length past))
    ))

(provide history%)