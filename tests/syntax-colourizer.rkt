#lang racket/base

(require rackunit
    rackunit/text-ui
    racket/list
    racket/class
    racket/match
    racket/hash
    (except-in "../editor/lexer.rkt" flatten)
    "../editor/lexer-colours.rkt")

                

#|
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
|#

(define-test-suite lex-tests
    (test-case
        "can lex a line containing only an identifier"
        (let* ([src (string->list "identifier")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? acc (set-colour '() src unknown-colour))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex a number"
        (let* ([src (string->list "1234")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? acc (set-colour '() src constant-colour))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex a single ("
        (let* ([src (string->list "(")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? (flatten acc) (flatten (set-colour '() src (get-bracket-colour 0))))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-bracket-counter line) 1)
                (check-equal? (saved-line-used-colours line) (hash 0 (get-bracket-colour 0)))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex a single )"
        (let* ([src (string->list ")")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? (flatten acc) (flatten (set-colour '() src invalid-bracket-colour)))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-bracket-counter line) -1)
                (check-equal? (saved-line-used-colours line) (hash 0 invalid-bracket-colour))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex a matching bracket pair"
        (let* ([src (string->list "()")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)]
                [colour (get-bracket-colour 0)])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? (flatten acc) (flatten 
                        (set-colour 
                            (set-colour '() (first src) colour)
                            (second src)
                            colour)))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-bracket-counter line) 0)
                (check-equal? (saved-line-used-colours line) (hash 0 colour 1 colour))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex an unbalanced bracket pair"
        (let* ([src (string->list ")(")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)]
                [colour1 invalid-bracket-colour]
                [colour2 (get-bracket-colour 0)])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? (flatten acc) (flatten 
                        (set-colour 
                            (set-colour '() (first src) colour1)
                            (second src)
                            colour2)))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-bracket-counter line) 0)
                (check-equal? (saved-line-used-colours line) (hash 0 colour1 1 colour2))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex an unmatched bracket pair"
        (let* ([src (string->list "(]")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)]
                [colour1 (get-bracket-colour 0)]
                [colour2 invalid-bracket-colour])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? (flatten acc) (flatten 
                        (set-colour 
                            (set-colour '() (first src) colour1)
                            (second src)
                            colour2)))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-bracket-counter line) 0)
                (check-equal? (saved-line-used-colours line) (hash 0 colour1 1 colour2))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex a more complicated bracket sequence"
        (let* ([src (string->list "((())(]))]]][")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)]
                [colours (list 
                    (get-bracket-colour 0)
                    (get-bracket-colour 1)
                    (get-bracket-colour 2)
                    (get-bracket-colour 2)
                    (get-bracket-colour 1)
                    (get-bracket-colour 1)
                    invalid-bracket-colour
                    (get-bracket-colour 1)
                    (get-bracket-colour 0)
                    invalid-bracket-colour
                    invalid-bracket-colour
                    invalid-bracket-colour
                    (get-bracket-colour 0))])
                    
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? (flatten acc) (flatten
                    (for/fold ([acc '()]) ([i src] [c colours])
                        (set-colour acc i c))))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-bracket-counter line) -3)
                (check-equal? (saved-line-used-colours line)
                    (make-immutable-hash 
                        (for/list ([i (in-range (length src))] [j colours])
                            (cons i j)))) 
                (check-equal? (saved-line-length line) (length src)))))


    (test-case 
        "can lex an identifier in brackets"
        (let* ([src (string->list "(test)")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? (flatten acc) (flatten 
                    (set-colour 
                        (set-colour 
                            (set-colour '() #\( (get-bracket-colour 0))
                            (string->list "test")
                            unknown-colour)
                        #\)
                        (get-bracket-colour 0))))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-bracket-counter line) 0)
                (check-equal? (saved-line-used-colours line) 
                    (hash 0 (get-bracket-colour 0) 5 (get-bracket-colour 0)))
                (check-equal? (saved-line-length line) (length src)))))

    
    (test-case 
        "can lex a matching bracket pair on multiple lines"
        (let* ([src0 (string->list "(")]
                [src1 (string->list ")")]
                [line (make-empty-saved-line 0)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line0 lines) (lex src0 '() 0 line lines)])
                (let-values ([(acc line1 lines) (lex src1 '() 0 (make-empty-saved-line 1) (add-line-to-accumulated line0 lines))])
                    (let ([line0 (first (accumulated-lines-lines lines))])
                        (check-equal? (flatten acc) (flatten (set-colour '() (first src1) (get-bracket-colour 0))))
                        (check-equal? (saved-line-bracket-counter line0) 1)
                        (check-equal? (saved-line-bracket-counter line1) -1)
                        (check-equal? (saved-line-used-colours line0) (hash 0 (get-bracket-colour 0)))
                        (check-equal? (saved-line-used-colours line1) (hash 0 (get-bracket-colour 0)))
                        (check-equal? (saved-line-length line0) (length src0))
                        (check-equal? (saved-line-length line1) (length src1))
                        (check-equal? (saved-line-matching-pairs line0) (hash (foreign-key 0) (matching-pair 1 0)))
                        (check-equal? (saved-line-matching-pairs line1) (hash 0 (matching-pair 0 0))))))))

    (test-case 
        "can lex a string"
        (let* ([src (string->list "\"hello world\"")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line lines) (lex src '() 0 line lines)])
                (check-equal? acc (set-colour '() src string-colour))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-length line) (length src)))))
        
)

(define rp #\))

(define (id x) x)

(define-test-suite helper-tests
    (test-case 
        "can-match finds a suitable match for one line"
        (let* ([src0 (reverse (string->list "(define (test)")) ]
                [line0 (saved-line 0 src0 (make-immutable-hash) 0 (make-immutable-hash) (length src0))])
            (let ([result (can-match? #\) (id (length (saved-line-characters line0))) 0 line0 '())])
                (check-true (match-result-success result))
                (check-eq? (match-result-character-index result) 8)
                (check-eq? (match-result-line-index result) 0))))

    (test-case 
        "store-match stores the match properly"
        (let* ([src0 (reverse (string->list "(define (test)"))]
                [line0 (saved-line 0 src0 (make-immutable-hash) 0 (make-immutable-hash) (length src0))])
            (let ([result (can-match? rp (id (length (saved-line-characters line0))) 0 line0 '())])
                (let-values ([(current previous) (store-match 13 result line0 '())])
                    (check-equal? 
                        (saved-line-matching-pairs current)
                        (hash 8 (matching-pair 0 13) 13 (matching-pair 0 8))
                        )))))


    (test-case 
        "can-match finds a suitable match for several lines"
        (let* ([src0 (reverse (string->list "(define (test)")) ]
                [line0 (saved-line 0 src0 (make-immutable-hash) 0 (make-immutable-hash) (length src0))]
                [src1 (reverse (string->list "(if (> 0 1)")) ]
                [line1 (saved-line 1 src1 (make-immutable-hash) 0 (make-immutable-hash) (length src1))]
                [src2 (reverse (string->list "1 0))")) ]
                [line2 (saved-line 2 src2 (make-immutable-hash) 0 (make-immutable-hash) (length src2))])
            (let ([result (can-match? rp (id (saved-line-length line0)) 0 line0 '())])
                (let-values ([(current0 previous0) (store-match 13 result line0 '())])
                    (let ([result (can-match? rp (id (saved-line-length line1)) 1 line1 (cons current0 previous0))])
                        (let-values ([(current1 previous1) (store-match 10 result line1 (cons current0 previous0))])
                            (let ([result (can-match? rp (sub1 (saved-line-length line2)) 2 line2 (cons current1 previous1))])
                                (let-values ([(current2 previous2) (store-match 3 result line2 (cons current1 previous1))])
                                    (let ([result (can-match? rp (id (saved-line-length line2)) 2 current2 previous2)]);line2 (cons current2 previous2))])
                                        (let-values ([(current previous) (store-match 4 result current2 previous2)]);line2 (cons current2 previous2))])
                                            (let ([line0-hash (hash 
                                                            13 (matching-pair 0 8) 
                                                            8 (matching-pair 0 13)
                                                            (foreign-key 0) (matching-pair 2 4))]
                                                    [line1-hash (hash 
                                                            10 (matching-pair 1 4) 
                                                            4 (matching-pair 1 10)
                                                            (foreign-key 0) (matching-pair 2 3))]
                                                    [line2-hash (hash 4 (matching-pair 0 0) 3 (matching-pair 1 0))])
                                                    (check-equal?
                                                        (saved-line-matching-pairs (second previous))
                                                        line0-hash)
                                                    (check-equal?
                                                        (saved-line-matching-pairs (first previous))
                                                        line1-hash)
                                                    (check-equal?
                                                        (saved-line-matching-pairs current)
                                                        line2-hash)

                                            ))))))))))) 
                                            
)

(run-tests lex-tests)
;(run-tests helper-tests)
            