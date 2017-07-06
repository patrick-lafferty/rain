#lang typed/racket/base/no-check 

(provide (all-defined-out))

(require "lexer-colours.rkt"
    racket/match
    (except-in racket/list flatten)
    racket/hash)

(define-type NestedCharList (Rec a (U Char (Listof a))))
(define (flatten [x : (Listof NestedCharList)]) : (Listof Char)
    (define (func)
        (for/fold ([acc : (Listof Char) '()]) ([i : NestedCharList x]) 
            (if (list? i)
                (for/fold ([acc acc]) ([j (flatten i)])
                    (cons j acc))
                (cons i acc))))
    (reverse (func)))

(define (get-bracket-colour [x : Integer]) : Integer 
    (vector-ref bracket-colours 
        (modulo (if (< x 0) 0 x) (vector-length bracket-colours))))

(define (set-colour [acc : (Listof Char)] 
                    [characters : (U Char (Listof Char))]
                    [colour : Integer]) : (Listof Char)
    (add-to-acc 
        (add-to-acc acc ;(reverse
                    (string->list (format "\e[38;5;~am" colour)));)
        (if (char? characters)
            (list characters)
            characters)))

(define (highlight 
            [acc : (Listof Char)] 
            [character : Char]) : (Listof Char)
    (add-to-acc 
        (add-to-acc 
            (add-to-acc acc (string->list "\e[48;5;183m")) 
            (list character))
        (string->list "\e[0m")))

(define (char-delimiter? [c : Char]) : Boolean
    (match c
        [#\( #t]
        [#\) #t]
        [#\[ #t]
        [#\] #t]
        [_ #f]))

(define (char-identifier? [c : Char]) : Boolean
    (not
        (or (char-whitespace? c)
            (char-delimiter? c))))

(define (add-to-acc 
            [acc : (Listof Char)] 
            [thing : (Listof Char)]) : (Listof Char)
    ;(if (null? acc)
    ;    thing
        (for/fold ([acc acc]) ([c thing])
            (cons c acc)));)

(define (char-not-quote? [c : Char]) : Boolean
    (not (eqv? #\" c)))

(struct matching-pair (
    [line : Integer] ;index to the matching line 
    [character : Integer] ;index to the matching character
    ) #:transparent)
    
(struct match-result (
    [success : Boolean] ;#t if there was a result, #f otherwise 
    [character-index : Integer] ;index of the matching character
    [line-index : Integer] ;index of the matching line
    [orig-line-index : Integer] ;index of the line with the requesting character
    ) #:transparent)

(struct foreign-key (
    [index : Integer] ;character index
    ) #:transparent)

(struct saved-line (
    [index : Integer] ;the unique index of the line
    [characters : (Listof Char)] ;list of printable characters to be lexed again
    [matching-pairs : (HashTable (U Integer foreign-key) matching-pair)];hash-set of character index to (line-index, character-index)
    [bracket-counter : Integer] ; sum of #-of-openers and #-of-closers
    [used-colours : (HashTable Integer Integer)];hash-set of character index to colour 
    [length : Integer];cached length
) #:transparent)

(define (make-empty-saved-line [index : Integer 0]) : saved-line
    (saved-line 
        index 
        '() 
        (make-immutable-hash)
        0
        (make-immutable-hash)
        0))

(struct accumulated-lines (
        [lines : (Listof saved-line)] ;list of previous lines
        [bracket-counter : Integer];sum of all lines' bracket counters
))

(define (make-empty-accumulated-lines) : accumulated-lines
    (accumulated-lines 
        '()
        0))

(define (add-line-to-accumulated 
            [line : saved-line]
            [accumulated : accumulated-lines]) : accumulated-lines
    (struct-copy accumulated-lines accumulated
        [lines (cons line (accumulated-lines-lines accumulated))]
        [bracket-counter (+ (saved-line-bracket-counter line) (accumulated-lines-bracket-counter accumulated))]))

(define (is-matching? 
            [a : Char] 
            [b : Char]) : Boolean
    (match a
        [#\( (eqv? b #\))]
        [#\[ (eqv? b #\])]
        [#\{ (eqv? b #\})]
        [_ #f]))

(define (get-used-colour 
            [result : match-result] 
            [line : saved-line]
            [lines : (Listof saved-line)]) : Integer
    (if (eqv? (match-result-line-index result) (saved-line-index line))
        (hash-ref (saved-line-used-colours line) 
                (match-result-character-index result))
        (if (null? lines)
            invalid-bracket-colour
            (get-used-colour result (first lines) (rest lines)))))

(define (get-character-at 
            [index : Integer] 
            [line : saved-line]) : Char
    ;characters are stored in reverse order
    (list-ref (saved-line-characters line) (- (saved-line-length line) index)))

;starting at character index in line, look for the appropriate ( or [
;that closes that character
(define (can-match? 
        [c : Char] 
        [index : Integer] 
        [orig-line-index : Integer] 
        [line : saved-line] 
        [lines : (Listof saved-line)]) : match-result
    (if (< index 1)
        (if (null? lines)
            (match-result #f 0 0 0)
            (let ([next-line (first lines)])
                (can-match? c (saved-line-length next-line) orig-line-index next-line (rest lines))))
        (if (is-matching? 
                (get-character-at index line)
                c)
            (if (hash-has-key? (saved-line-matching-pairs line) (sub1 index))
                (can-match? c (sub1 index) orig-line-index line lines)
                (if (hash-has-key? (saved-line-matching-pairs line) (foreign-key (sub1 index)))
                    (can-match? c (sub1 index) orig-line-index line lines)
                    (match-result #t (sub1 index) (saved-line-index line) orig-line-index)))
            (can-match? c (sub1 index) orig-line-index line lines))))

(define (update-previous 
            [result : match-result] 
            [ch-id : Integer] 
            [ln-id : Integer]
            [previous-lines : (Listof saved-line)]) : (Listof saved-line)
    (if (eqv? (match-result-line-index result) 
                (saved-line-index (first previous-lines)))
        (let ([updated-line (struct-copy saved-line (first previous-lines)
            [matching-pairs 
                (hash-set (saved-line-matching-pairs (first previous-lines))
                    (foreign-key (match-result-character-index result))
                        (matching-pair
                            ln-id ch-id))])])
            (cons updated-line (rest previous-lines)))
        (let ([previous (update-previous result ch-id ln-id (rest previous-lines))])
            (cons (first previous-lines) previous))))

(define (store-match 
            [character-index : Integer] 
            [result : match-result]
            [current-line : saved-line]
            [previous-lines : (Listof saved-line)]) : (Values saved-line (Listof saved-line))
    ;store closer first, then opener possibly on other line
    (if (eqv? (match-result-orig-line-index result) (saved-line-index current-line))
        (let ([updated-line (struct-copy saved-line current-line
                [matching-pairs 
                    (let ([hs
                        (hash-set (saved-line-matching-pairs current-line) 
                            character-index 
                                (matching-pair 
                                    (match-result-line-index result) 
                                    (match-result-character-index result)))])
                        
                        (if (eqv? (saved-line-index current-line) (match-result-line-index result))
                            (hash-set hs
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

;TODO: need to check old matches from previous lines
;to make sure expired ones arent needed anymore
;(eg lexing current line, had a match but the 
;user deleted the bracket so now the match
;on some previous line isn't used anymore

;TODO: for identifiers add an expansion point at the end that
;allows an expander like autocomplete to add text
(define (lex 
            [characters : (Listof Char)]
            [acc : (Listof Char)]
            [index : Integer] 
            [line : saved-line]
            [lines : accumulated-lines]) : (Values (Listof Char) saved-line accumulated-lines)
    (if (null? characters)
        (values acc line lines)
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
                                    [bracket-counter (sub1 (saved-line-bracket-counter line))]
                                    [length (add1 (saved-line-length line))])]
                            [result (can-match? c index (saved-line-index updated-line) updated-line (accumulated-lines-lines lines))])
                        (if (match-result-success result)
                            (let-values ([(current previous) 
                                    (store-match index result updated-line (accumulated-lines-lines lines))])
                                (let* ([colour (get-used-colour result current previous)]
                                        [coloured-line
                                            (struct-copy saved-line current
                                                [used-colours (hash-set (saved-line-used-colours current)
                                                    index colour)])]
                                        [acc (set-colour acc c colour)]
                                        [updated-acc-lines 
                                            (struct-copy accumulated-lines lines
                                                [lines previous])])
                                    (lex (rest characters) acc (add1 index) coloured-line updated-acc-lines)))
                            (let ([coloured-line 
                                        (struct-copy saved-line updated-line
                                            [used-colours (hash-set (saved-line-used-colours updated-line)
                                                index invalid-bracket-colour)])]
                                    [acc (set-colour acc c invalid-bracket-colour)])
                                (lex (rest characters) acc (add1 index) coloured-line lines))))

                    ]
                [(? char-numeric?)
                    ;constant number
                    (let-values ([(number remaining) (splitf-at characters char-numeric?)])
                        (let ([updated-line
                            (struct-copy saved-line line
                                [characters (for/fold ([acc : (Listof Char) (saved-line-characters line)]) 
                                                    ([i : Char number]) 
                                                (cons i acc))]
                                [length (+ (saved-line-length line) (length number))])]
                              [acc (set-colour acc number constant-colour)])
                            (lex (drop characters (length number)) acc (+ index (length number)) updated-line lines)))
                    ]
                [(? char-whitespace?)
                    ;single whitespace character
                    (let ([updated-line
                        (struct-copy saved-line line
                            [characters (cons c (saved-line-characters line))]
                            [length (add1 (saved-line-length line))])])
                        (lex (rest characters) (cons c acc) (add1 index) updated-line lines))
                    ]
                [#\"
                    ;constant string
                    (let*-values (
                            [(string remaining) (splitf-at (rest characters) char-not-quote?)]
                            [(remaining add-closing-quote?)
                                (if (null? remaining) (values remaining #f)
                                    (if (eqv? #\" (first remaining)) (values (rest remaining) #t)
                                        (values remaining #f)))])
                        (let* ([quoted
                                (if add-closing-quote? (flatten (list #\" string #\"))
                                    (flatten (list #\" string)))]
                               [acc (set-colour acc quoted string-colour)]
                               [updated-line
                                (struct-copy saved-line line
                                    [characters (for/fold ([acc : (Listof Char) (saved-line-characters line)])
                                                        ([i : Char quoted])
                                                    (cons i acc))]
                                    [length (+ (saved-line-length line) (length quoted))])])
                            (lex (drop characters (length quoted)) acc (+ index (length quoted)) updated-line lines)))
                    ]
                [_
                    ;identifier
                    (let-values ([([identifier : (Listof Char)] [remaining : (Listof Char)]) 
                            (splitf-at characters char-identifier?)])
                        (let* ([symbol (string->symbol (list->string identifier))]
                                [colour unknown-colour]
                                [updated-line 
                                    (struct-copy saved-line line
                                        [characters (for/fold ([acc : (Listof Char) (saved-line-characters line)])
                                                            ([i : Char identifier])
                                                        (cons i acc))]
                                        [length (+ (saved-line-length line) (length identifier))])]
                                [acc (set-colour acc identifier colour)])
                            (lex (drop characters (length identifier)) acc (+ index (length identifier)) updated-line lines)))
                            
                    ]))))