#lang racket/base

(require rackunit
    rackunit/text-ui
    racket/list
    racket/hash)

(struct saved-line (
    characters ;list of printable characters to be lexed again
    matching-pairs ;hash-set of character index to (line-index, character-index)
    bracket-counter ; sum of #-of-openers and #-of-closers
    used-colours ;hash-set of character index to colour 
))

(define (lex line acc saved-line previous-lines)
    (if (null? lst)
        (values acc saved-line)
        (match (first lst)
            [(or #\( #\[)
                ;opener
                ]
            [(or #\) #\])
                ;closer
                ]
            [(? char-numeric?)
                ;constant number
                ]
            [(? char-whitespace?)
                ;single whitespace character
                ]
            [#\"
                ;constant string
                ]
            [_
                ;identifier
                ])))

(define pretty-printer%
    (class object%
        (super-new)

        (define/public (print-line line row)
            (let* ([current-line (saved-line '() (make-hash) 0 (make-hash))]
                    [lexed (lex line '() saved-line)])


(define-test-suite line-tests
    (test-case 
        "makes a valid line struct for each line"
        ;(check-equal? ....)
    )

(run-tests line-tests)