#lang typed/racket/base

(require "../functional/maybe.rkt"
    racket/list
    racket/match)
    ;"../interpreter.rkt")

(define-type Trie trie-node)
(struct trie-node (
    [label : (Maybe (Listof Char))]
    ;[children : (HashTable Char Trie)])
    [children : (Listof Trie)])
    #:transparent)   

(define known-identifiers (trie-node (some '()) '()))
#|
find tellephone (te (st ll)) 0

  te llephone '()
  find llephone (st ll) 0
    find llephone (ll) 1
        ll ephone ()
        find ephone ()
            none (couldnt find in any children)
        if has children
            add-leaf
            replace-node
        




|#
#|(struct find-result (
    [common : (Listof Char)] ;common prefix between target identifier and child label
    [rest-identifier : (Listof Char)] ; the remaining part of identifier after common prefix
    [rest-child : (Listof Char)] ;the remaining part of child label after common prefix
    [child-index : Integer] ;the index of the child that this identifier matched
    ;[breadcrumbs : (Listof Integer)]
) #:transparent)

(: fmap-cons (-> find-result (Maybe (Listof find-result)) (Maybe (Listof find-result))))
(define (fmap-cons a b)
    (some 
        (match b
            [(some x) (cons a x)]
            [(none) (cons a empty)])))|#
#|
how does this work:

if x and y share a common prefix,
-if (rest x) and any child of y share a common prefix ^

test 
+ tell
-shares 'te', trie is just test
replace 'test' with 'te' ['st', 'll']

what does find-result need:

node:
-common prefix
-child index to replace

leaf:
-rest-id
-rest-child

type find-result =
    | node common-prefix child-index
    | leaf rest-id rest-child

(find-result te 1
    (find-result l 
        l
        lephone)

[te [st, ll]] => [te [st, l [l, ephone]]]
[te [st, ll]] => [te, [st, ll, am]]
+telephone
-shares 'te'
-shares 'l'
replace 'll' with 'l'['l', 'ephone']

+television
-shares 'te'
-shares 'l'
-shares 'e'
'te' ['st', 'l' ['e' ['phone', 'vision'], 'l']

+telling
'te' ['st, 'l'
    ['e' ['phone', 'vision']]
    ['l' [null, 'ing']]

|#
#|(define (find-common-prefix 
            [identifier : (Listof Char)] 
            [children : (Listof Trie)]
            [child-index : Integer]) : (Maybe (Listof find-result))
    (if (null? children)
        (none)
        (let-values ([(common rest-id rest-child) 
                        (split-common-prefix identifier (trie-node-label (first children)))])
            (if (null? common)
                (find-common-prefix identifier (rest children) (add1 child-index))
                ;(some (find-result common rest-id rest-child child-index))))))
                (if (null? rest-id)
                    ;(some (cons (find-result common rest-id rest-child child-index)) empty)
                    (fmap-cons (find-result common rest-id rest-child child-index) (none))
                    ;(cons (find-result common rest-id rest-child child-index) (find-common-prefix rest-id (trie-node-children (first children)))))))))
                    (fmap-cons 
                        (find-result common rest-id rest-child child-index) 
                        (find-common-prefix rest-id 
                            (trie-node-children (first children))
                            0
                            )))))))
                            |#
;type find-result =
    ;| node common-prefix child-index
    ;| leaf rest-id rest-child
#|

type find-result =
    | 'exists
    | add-leaf id
    | split-node common rest-id rest-child
    | update-child index

'te' ['st', 'lling'] + tell

(update-child 1) (replace-node 'll' '' 'ing)

'te' ['st', 'll'] + telephone

((update-child 1) (split-node 'l' 'ephone' 'l'))

'te' ['st', 'l' ['l', 'ephone']] + telling

(update-child 1) (add-leaf ing)

'te' ['st', 'l' ['l' [null, 'ing'], 'ephone']] + television

(update-child 1) (update-child 1) (split-node 'e' 'vision' 'phone')

|#
(struct update-child (
    [child-index : Integer]
) #:transparent)

(struct replace-node (
    [child-index : Integer]
    [common-prefix : (Listof Char)]
    [rest-identifier : (Listof Char)]
    [rest-child : (Listof Char)]
    ) #:transparent)

(struct add-leaf (
    [child-index : Integer]
    [rest-identiifer : (Listof Char)]
) #:transparent)

(define-type FindResult (U update-child replace-node add-leaf 'exists))

(define (find
    [identifier : (Listof Char)]
    [trie : Trie]
    [parent-index : Integer]) : (Maybe (Listof FindResult))

    (define (find-children
        [identifier : (Listof Char)]
        [children : (Listof Trie)]
        [child-index : Integer]
        [parent-index : Integer]) : (Pair Integer (Maybe (Listof FindResult)))
        (if (null? children)
            (cons -1 (some (list (add-leaf parent-index identifier))))
            (let ([result (find identifier (first children) parent-index)])
                (match result
                    [(some _ ) (cons child-index result)]
                    [(none) (find-children identifier (rest children) (add1 child-index) parent-index)]))))

    (match (trie-node-label trie) 
        ;root - check children
        [(some '()) 
            (let ([result (find-children identifier (trie-node-children trie) 0 0)])
                (match result 
                    ;[(cons child-index result) (some (list (update-child child-index) result))]))]
                    [(cons _ (some (list 'exists))) (some (list 'exists))]
                    [(cons -1 (some change)) (some change)]
                    [(cons x (some change)) (some (cons (update-child x) change))]))]
        [(some label)
            (let-values ([(common rest-id rest-label)
                        (split-common-prefix identifier label)])
                (if (null? common)
                    ;nothing in common, go back
                    (none)
                    ;something in common
                    (if (null? rest-id)
                        ;end of search word reached
                        (if (null? rest-label)
                            ;end of label reached
                            (some (list 'exists))
                            ;end of search word but label still has more letters, split label
                            (some (list (replace-node parent-index common rest-id rest-label))))
                        ;more left in search word
                        (if (null? rest-label)
                            ;more left in search word but label is a proper prefix of search word
                            ;(some (list (replace-node parent-index common rest-id rest-child)))
                            (let ([result (find-children rest-id (trie-node-children trie) 0 parent-index)])
                                (match result
                                    [(cons _ (some (list 'exists))) (some (list 'exists))]
                                    [(cons -1 (some change)) (some change)]
                                    [(cons x (some change)) (some (cons (update-child x) change))]))
                            ;more left in both, find a child that can complete the search word
                            ;(let ([result (find rest-id )]))
                            (some (list (replace-node parent-index common rest-id rest-label)))))))]))
       ;telll + tell => tel ll l             

        ;end of word
        ;[(none) (some (list (replace-node parent-index identifier)))]


    #|(if (null? (trie-node-children trie))
        (some (list (add-leaf parent-index identifier)))
        (let-values ([(common rest-id rest-child)
                        (split-common-prefix identifier (trie-node-label (first children)))])
            (if (null? common)
                ;this child has nothing in common, look at the other children
                (let ([result (find identifier (rest children) (add1 child-index))])
                    (match result 
                        [(some _ ) result]
                        ;should never be none
                        [(none) (some (list (add-leaf child-index rest-id)))]))
                (if (null? rest-id)
                    (if (null? rest-child)
                        (some (list 'exists))
                        (some (list (replace-node child-index common rest-id rest-child))))
                    ;we have something in common, theres still something in id remaining
                    (let ([result (find rest-id (trie-node-children (first children)) 0)])
                        (match result
                            [(some result)
                                (match result
                                    [(list 'exists ) (some (list 'exists))]
                                    [(list (replace-node index _ _ _))
                                        (some (cons (update-child index) result))]
                                    [_ 
                                        (some (cons (update-child child-index) result))])]
                            [(none)
                                (if (null? (trie-node-children (first children)))
                                    (some (list (replace-node child-index common rest-id rest-child)))
                                    (some (list (add-leaf child-index rest-id))))]
                            )))))))|#
                        

#|(define test (trie-node empty
    (list (trie-node (string->list "te") 
        (list (trie-node (string->list "st") empty)
                (trie-node (string->list "ll") empty) 
    
    )))))
|#
#|
te 
    [st ll]

+telephone

(update-child 1) (replace-node 1 l ephone l)

te
    [st l]
        [lephone l]

|#
;(find (string->list "tell") (trie-node-children test) 0) 
(define (replace 
        [trie : Trie]
        [breadcrumbs : (Maybe (Listof FindResult))]) : Trie
    (match breadcrumbs
        [(none) trie]
        [(some (list 'exists)) trie]
        [(some (cons (update-child index) remaining))
            (let ([children (trie-node-children trie)])
                   ;[trie (if (< index 0) trie (list-ref children index))])
                (struct-copy trie-node trie
                    [children (list-set children index (replace (list-ref children index) (some remaining)))]))]
        [(some (cons (replace-node child-index common rest-id rest-child) _))
            (let ([replacement 
                    (trie-node (some common) (list 
                        (trie-node (if (null? rest-id) (none) (some rest-id)) empty)
                        (trie-node (some rest-child) (trie-node-children trie))))])
                replacement)]
        [(some (list (add-leaf child-index rest-id)))
            (struct-copy trie-node trie
                [children (cons (trie-node (some rest-id) empty) (trie-node-children trie))])]))
        #|[(cons (find-result common rest-id rest-child index) empty)
            (let* ([child-to-replace (list-ref children index)])

                (struct-copy trie-node child-to-replace
                    [label common]
                    [children (list (trie-node rest-id '()#|empty|#) (trie-node rest-child '()#|empty|#))]))]

        [(cons (find-result common _ _ index) others)
            (let* ([child-to-replace (list-ref children index)]
                    [childs-children (trie-node-children child-to-replace)])

                (struct-copy trie-node child-to-replace
                    [label common]
                    [children (list-set children index (replace childs-children (rest breadcrumbs)))]))]))
                    |#

#|(define (insert 
    [identifier : (Listof Char)]
    [trie : Trie]) : Trie
    
    (let ([children (trie-node-children trie)])
        (if (null? children)
            (struct-copy trie-node trie 
                [children (list (trie-node identifier empty))])
            (match (find-common-prefix identifier children 0)
                [(some breadcrumbs)
                    ;(struct-copy trie-node trie
                    (replace children breadcrumbs)]
|#

(define (insert
    [identifier : (Listof Char)]
    [trie : Trie]) : Trie
    (let ([breadcrumbs (find identifier trie 0)])
        (printf "breadcrumbs: ~v~n" breadcrumbs)
        (replace trie breadcrumbs)))
                #|[(some (find-result common rest-id rest-child index))
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
                                [children (list-set children index replaced-subtrie)])))]|#
                    
              #|  [(none)
                    (let ([new-child (trie-node identifier empty)])
                        (struct-copy trie-node trie
                            [children (cons new-child children)]))]))))
|#
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
(for/fold ([trie : Trie (trie-node (some '()) empty)])
    ([id : Symbol interpreter-keywords])
    (writeln trie)
    (writeln "")
    (insert (string->list (symbol->string id)) trie)))

;(define (autocomplete characters)
