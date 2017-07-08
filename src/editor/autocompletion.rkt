#lang typed/racket/base

(require "../functional/maybe.rkt"
    racket/list
    racket/match)
    ;"../interpreter.rkt")

(define-type Trie trie-node)
(struct trie-node (
    [label : (Listof Char)]
    ;[children : (HashTable Char Trie)])
    [children : (Listof Trie)])
    #:transparent)   

(define known-identifiers (trie-node '() '()))

(struct find-result (
    [common : (Listof Char)] ;common prefix between target identifier and child label
    [rest-identifier : (Listof Char)] ; the remaining part of identifier after common prefix
    [rest-child : (Listof Char)] ;the remaining part of child label after common prefix
    [child-index : Integer] ;the index of the child that this identifier matched
))

(define (find-common-prefix 
            [identifier : (Listof Char)] 
            [children : (Listof Trie)]
            [child-index : Integer]) : (Maybe find-result)
    (if (null? children)
        (none)
        (let-values ([(common rest-id rest-child) 
                        (split-common-prefix identifier (trie-node-label (first children)))])
            (if (null? common)
                (find-common-prefix identifier (rest children) (add1 child-index))
                (some (find-result common rest-id rest-child child-index))))))


(define (insert 
    [identifier : (Listof Char)]
    [trie : Trie]) : Trie
    
    (let ([children (trie-node-children trie)])
        (if (null? children)
            (struct-copy trie-node trie 
                [children (list (trie-node identifier empty))])
            (match (find-common-prefix identifier children 0)
                [(some (find-result common rest-id rest-child index))
                    (if (null? rest-id)
                        (let ([replaced-child 
                                (trie-node common 
                                    (list (trie-node rest-id empty) 
                                            (trie-node rest-child (trie-node-children (list-ref children index)))))])
                            (struct-copy trie-node trie
                                [children (list-set children index replaced-child)]))
                        (let* ([result (insert rest-id (list-ref children index))]
                                [replaced-subtrie 
                                (struct-copy trie-node result
                                    [label common]
                                    [children (cons (trie-node rest-child empty) (trie-node-children result))])])
                            (struct-copy trie-node trie
                                [children (list-set children index replaced-subtrie)])))]
                    
                [(none)
                    (let ([new-child (trie-node identifier empty)])
                        (struct-copy trie-node trie
                            [children (cons new-child children)]))]))))

(: print-trie (->* (Trie) (Integer) Void))
(define (print-trie 
    trie [indent 0])
    ;[trie : Trie] 
    ;[indent : Integer 0]) : Void
    (printf "~a~v~n" (make-string indent #\space) (trie-node-label trie))
    (for ([child : Trie (trie-node-children trie)])
        (print-trie child (add1 indent))))
#|
add test:
test
----
add tell:
te
    st ll
--------
add telephone:
te
    st l
        l   lephone
(print-trie 
    (insert (string->list "telephone")
        (insert (string->list "tell") 
            (insert (string->list "test") known-identifiers))))
|#
(define interpreter-keywords (list 
    'if
    'or
    'and
    'cond
    'set
    'define-syntax
    'define
    'defile
    'lambda
    'quote
    'let
    'let*
    'letrec
    'begin
    'when
    'unless
    ))
(print-trie
(for/fold ([trie : Trie (trie-node empty empty)])
    ([id : Symbol interpreter-keywords])
    (insert (string->list (symbol->string id)) trie)))

;(define (autocomplete characters)
