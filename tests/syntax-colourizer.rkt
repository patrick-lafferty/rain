#lang racket/base

(require rackunit
    rackunit/text-ui
    racket/list
    racket/class
    racket/match
    racket/hash)

(define bracket-colours #(68 100 160))
(define (get-bracket-colour x) (vector-ref bracket-colours (modulo x (vector-length bracket-colours))))

(define (set-colour acc character x) 
    (add-to-acc (add-to-acc acc (string->list (format "\e[38;5;~am" x))) character))

(define (highlight acc character)
    (add-to-acc 
        (add-to-acc 
            (add-to-acc acc (
                   string->list "\e[48;5;183m")) 
            character) 
        normal))

(define normal (string->list "\e[0m"))

(define constant-colour 82)
(define string-colour 173)
(define special-form-colour 125)
(define identifier-colour 27)
(define unknown-colour 211)
(define invalid-bracket-colour 52)

(define (char-delimiter? c)
    (match c
        [#\( #t]
        [#\) #t]
        [#\[ #t]
        [#\] #t]
        [_ #f]))

(define (char-identifier? c)
    (not
        (or (char-whitespace? c)
            (char-delimiter? c))))

(define (add-to-acc acc thing)
    (if (null? acc)
        thing
        (cons acc thing)))

(define (char-not-quote? c)
    (not (eqv? #\" c)))

(struct saved-line (
    index ; integer index
    characters ;list of printable characters to be lexed again
    matching-pairs ;hash-set of character index to (line-index, character-index)
    bracket-counter ; sum of #-of-openers and #-of-closers
    used-colours ;hash-set of character index to colour 
    length ;cached length
) #:transparent)

(define (make-empty-saved-line)
    (saved-line 
        0 
        '() 
        (make-immutable-hash)
        0
        (make-immutable-hash)
        0))

(struct accumulated-lines (
        lines ;list of previous lines
        bracket-counter ;sum of all lines' bracket counters
))

(define (make-empty-accumulated-lines)
    (accumulated-lines 
        '()
        0))

(struct match-result (success character-index line-index orig-line-index) #:transparent)

(define (is-matching? a b)
    (match a
        [#\( (eqv? b #\))]
        [#\[ (eqv? b #\])]
        [#\{ (eqv? b #\})]
        [_ #f]))

(define (get-used-colour result line lines)
    (if (eqv? (match-result-line-index result) (saved-line-index line))
        (hash-ref (saved-line-used-colours line) (match-result-character-index))
        (if (null? lines)
            invalid-bracket-colour
            (get-used-colour result (first lines) (rest lines)))))

(define (get-character-at index line)
    ;characters are stored in reverse order
    ;(printf "gca ~v ~v~n" index (saved-line-length line))
    (list-ref (saved-line-characters line) (- (saved-line-length line) index)))

;starting at character index in line, look for the appropriate ( or [
;that closes that character

(define (can-match? c index orig-line-index line lines)
    (if (< index 1)
        (if (null? lines)
            (match-result #f 0 0 0)
            (let ([next-line (first lines)])
                (can-match? c (id (saved-line-length next-line)) orig-line-index next-line (rest lines))))
        (if (is-matching? 
                (get-character-at index line)
                c)
            (if (hash-has-key? (saved-line-matching-pairs line) (sub1 index))
                (can-match? c (sub1 index) orig-line-index line lines)
                (if (hash-has-key? (saved-line-matching-pairs line) (foreign-key (sub1 index)))
                    (can-match? c (sub1 index) orig-line-index line lines)
                    (match-result #t (sub1 index) (saved-line-index line) orig-line-index)))
            (can-match? c (sub1 index) orig-line-index line lines))))

(struct matching-pair (line character) #:transparent)
(struct foreign-key (index) #:transparent)

(define (update-previous result ch-id ln-id previous-lines)
    (if (eqv? (match-result-line-index result) 
                (saved-line-index (first previous-lines)))
        (let ([updated-line (struct-copy saved-line (first previous-lines)
            [matching-pairs 
                (hash-set* (saved-line-matching-pairs (first previous-lines))
                    (foreign-key (match-result-character-index result))
                        (matching-pair
                            ln-id ch-id))])])
            (cons updated-line (rest previous-lines)))
        (let ([previous (update-previous result ch-id ln-id (rest previous-lines))])
            (cons (first previous-lines) previous))))

(define (store-match character-index result current-line previous-lines)
    ;store closer first, then opener possibly on other line
    (if (eqv? (match-result-orig-line-index result) (saved-line-index current-line))
        (let ([updated-line (struct-copy saved-line current-line
                [matching-pairs 
                    (let ([hs
                        (hash-set* (saved-line-matching-pairs current-line) 
                            character-index 
                                (matching-pair 
                                    (match-result-line-index result) 
                                    (match-result-character-index result)))])
                        
                        (if (eqv? (saved-line-index current-line) (match-result-line-index result))
                            (hash-set* hs
                                (match-result-character-index result)
                                    (matching-pair 
                                        (saved-line-index current-line) 
                                        character-index))
                            hs))])]
                [previous 
                    (if (null? previous-lines) 
                        '() 
                        (if (eqv? (match-result-line-index result) (saved-line-index current-line))
                            previous-lines
                            (update-previous result character-index 
                                (saved-line-index current-line) previous-lines)))])
            (values updated-line previous))
        (let-values ([(current previous) 
                        (store-match character-index result (first previous-lines) (rest previous-lines))])
            (values current-line (cons current previous)))))
                


(define (lex characters acc index line lines)
    (if (null? characters)
        (values acc line)
        (let ([c (first characters)])
            (match c
                [(or #\( #\[)
                    ;opener
                    (let ([colour (get-bracket-colour 
                            (+ (accumulated-lines-bracket-counter lines)
                                (saved-line-bracket-counter line)))])
                        (let ([updated-line 
                                (struct-copy saved-line line 
                                        [characters (cons c (saved-line-characters line))]
                                        [bracket-counter (add1 (saved-line-bracket-counter line))]
                                        [length (add1 (saved-line-length line))]
                                        [used-colours (hash-set (saved-line-used-colours line)
                                                index colour)]
                                    )]
                                [acc (set-colour acc c colour)])
                            (lex (rest characters) acc (add1 index) updated-line lines)))
                    ]
                [(or #\) #\])
                    ;closer

                    (let* ([updated-line
                                (struct-copy saved-line line
                                    [characters (cons c (saved-line-characters line))]
                                    [bracket-counter (add1 (saved-line-bracket-counter line))]
                                    [length (add1 (saved-line-length line))])]
                            [result (can-match? c index (saved-line-index updated-line) (accumulated-lines-lines lines))])
                        (if (match-result-success result)
                            (let-values ([(current previous) 
                                    (store-match index result updated-line (accumulated-lines-lines lines))])
                                (let* ([colour (get-used-colour result updated-line lines)]
                                        [coloured-line
                                            (struct-copy saved-line updated-line
                                                [used-colours (hash-set (saved-line-used-colours line)
                                                    index colour)])]
                                        [acc (set-colour acc c colour)]
                                        [updated-acc-lines 
                                            (struct-copy accumulated-lines lines
                                                [lines previous])])
                                    (lex (rest characters) acc (add1 index) coloured-line updated-acc-lines)))
                            (lex (rest characters) acc (add1 index) updated-line lines)))

                    ]
                [(? char-numeric?)
                    ;constant number
                    (let-values ([(number remaining) (splitf-at characters char-numeric?)])
                        (let ([updated-line
                            (struct-copy saved-line line
                                [characters (for/fold ([acc (saved-line-characters line)]) ([i number]) (cons i acc))]
                                [length (+ (saved-line-length line) (length number))])]
                              [acc (set-colour acc number constant-colour)])
                            (lex (drop characters (length number)) acc (+ index (length number)) updated-line lines)))
                        ;colour
                        ;update line
                        ;lex
                    ]
                [(? char-whitespace?)
                    ;single whitespace character
                    (let ([updated-line
                        (struct-copy saved-line line
                            [characters (cons c (saved-line-characters line))]
                            [length (add1 (saved-line-length line))])])
                        (lex (rest characters) acc (add1 index) updated-line lines))
                    ]
                [#\"
                    ;constant string
                    (void)
                    ]
                [_
                    ;identifier
                    (let-values ([(identifier remaining) (splitf-at characters char-identifier?)])
                        (let* ([symbol (string->symbol (list->string identifier))]
                                [colour unknown-colour]
                                [updated-line 
                                    (struct-copy saved-line line
                                        [characters (for/fold ([acc (saved-line-characters line)])
                                                            ([i identifier])
                                                        (cons i acc))]
                                        [length (+ (saved-line-length line) (length identifier))])]
                                [acc (set-colour acc identifier colour)])
                            (lex (drop characters (length identifier)) acc (+ index (length identifier)) updated-line lines)))
                            
                    ]))))
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
            (let-values ([(acc line) (lex src '() 0 line lines)])
                (check-equal? acc (set-colour '() src unknown-colour))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex a number"
        (let* ([src (string->list "1234")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line) (lex src '() 0 line lines)])
                (check-equal? acc (set-colour '() src constant-colour))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-length line) (length src)))))

    (test-case 
        "can lex a single ("
        (let* ([src (string->list "(")]
                [line (make-empty-saved-line)]
                [lines (make-empty-accumulated-lines)])
            (let-values ([(acc line) (lex src '() 0 line lines)])
                (check-equal? (flatten acc) (flatten (set-colour '() src (get-bracket-colour 0))))
                (check-equal? (saved-line-characters line) (reverse src))
                (check-equal? (saved-line-bracket-counter line) 1)
                (check-equal? (saved-line-used-colours line) (hash 0 (get-bracket-colour 0)))
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
            