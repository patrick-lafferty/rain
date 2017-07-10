#|
MIT License
Copyright (c) 2017 Patrick Lafferty
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
#lang typed/racket/base/no-check

(provide autocomplete
    add-symbol)

(require "../functional/maybe.rkt"
    racket/list
    racket/match)
    ;"../interpreter.rkt")

(require/typed 
    "../env.rkt"
    [get-all-mapped-symbols (-> (Listof Symbol))])

(define-type Trie trie-node)
(struct trie-node (
    [label : (Maybe (Listof Char))]
    [children : (Listof Trie)]
    [end-of-word : Boolean]
    )#:transparent)   

(define known-identifiers (trie-node (some '()) '() #f))

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

(struct exists (
    [child-index : Integer]
)#:transparent)

(define-type FindResult (U update-child replace-node add-leaf exists))

(define print? #f)

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
        [(none) 
            (if (null? identifier)
                (some (list (exists parent-index)))
                (none))]

        [(some '()) 
            (let ([result (find-children identifier (trie-node-children trie) 0 0)])
                (match result 
                    [(cons -1 (some change)) (some change)]
                    [(cons x (some change))
                        (match change 
                            [(cons (exists y) remaining) (some (cons (exists x) change))]
                            [_ (some (cons (update-child x) change))])]))]
        [(some label)
            (let-values ([(common rest-id rest-label)
                        (split-common-prefix identifier label)])
                (if (null? common)
                    ;nothing in common, go back
                    (begin 
                    (when print? (printf "common null: ~v ~v~n" rest-id rest-label))
                    (none)
                    )
                    ;something in common
                    (if (null? rest-id)
                        ;end of search word reached
                        (begin
                        (when print? (printf "rest-id null: ~v ~v~n" common rest-label))
                        (if (null? rest-label)
                            ;end of label reached
                            (begin (when print? (printf "rest-label null: ~v ~v ~v~n" common identifier label))
                            (some (cons (exists parent-index) '()))
                            )
                            ;end of search word but label still has more letters, split label
                            (some (cons (replace-node parent-index common rest-id rest-label) '())))
                        )
                        ;more left in search word
                        (begin (when print? (printf "rest-id not null: ~v ~v ~v~n" common rest-id rest-label))
                        (if (null? rest-label)
                            ;more left in search word but label is a proper prefix of search word
                            (let ([result (find-children rest-id (trie-node-children trie) 0 parent-index)])
                                (when print? (printf "result ~v~n" result))
                                (match result
                                    [(cons -1 (some change)) (some change)]
                                    [(cons x (some change))
                                        (match change 
                                            [(cons (exists y) remaining) (some (cons (exists x) change))]
                                            [_ (some (cons (update-child x) change))])]))
                            (some (cons (replace-node parent-index common rest-id rest-label) '())))
                            ))))]))

(define (replace 
        [trie : Trie]
        [breadcrumbs : (Maybe (Listof FindResult))]) : Trie
    (match breadcrumbs
        [(none) trie]
        [(some (cons (exists _) _)) trie]
        [(some (cons (update-child index) remaining))
            (let ([children (trie-node-children trie)])
                (struct-copy trie-node trie
                    [children (list-set children index (replace (list-ref children index) (some remaining)))]))]
        [(some (cons (replace-node child-index common rest-id rest-child) _))
            (let ([replacement 
                    (trie-node (some common) (list 
                        (trie-node (if (null? rest-id) (none) (some rest-id)) empty #t)
                        (trie-node (some rest-child) (trie-node-children trie) (trie-node-end-of-word trie)))
                        #f
                        )])
                replacement)]
        [(some (list (add-leaf child-index rest-id)))
            (struct-copy trie-node trie
                [children (cons (trie-node (some rest-id) empty #t) (trie-node-children trie))])]))

(define (insert
    [identifier : (Listof Char)]
    [trie : Trie]) : Trie
    (let ([breadcrumbs (find identifier trie 0)])
        (replace trie breadcrumbs)))

(: print-trie (->* (Trie) (Integer) Void))
(define (print-trie 
    trie [indent 0])
    (printf "~a~v~v~n" (make-string indent #\space) (trie-node-label trie) (trie-node-end-of-word trie))
    (for ([child : Trie (trie-node-children trie)])
        (print-trie child (add1 indent))))

(define interpreter-keywords (list 
    'if
    'or
    'and
    'cond
    'set
    'define-syntax
    'define
    'lambda
    'quote
    'let
    'let*
    'letrec
    'begin
    'when
    'unless
    
    'string
    'string->list
    'string-append
    ))

;TODO: have seperate completion tries for each interpreter environment
;and one for the main racket stuff (this one)

;TODO: consider allowing let/let* etc completion local to that let

(let ([thing
        (for/fold ([trie : Trie (trie-node (some '()) empty #f)])
            ([id : Symbol interpreter-keywords])
            (insert (string->list (symbol->string id)) trie))])
    (let ([thing 
        (for/fold ([trie : Trie thing])
                ([id : Symbol (get-all-mapped-symbols)])
            (insert (string->list (symbol->string id)) trie))])
        
        (set! known-identifiers thing)
))

;TODO: replace with proper hierarchy of tries
(define (add-symbol
    [id : Symbol]) : Void
    (set! known-identifiers 
        (insert (string->list (symbol->string id)) known-identifiers)))

(define (add-prefix 
    [prefix-to-add : (Maybe (Listof Char))]
    [trie : Trie]) : Trie
    (let* ([prefix-label
            (match prefix-to-add
                [(some x) x]
                [(none) '()])]
            [trie-label 
                (match (trie-node-label trie)
                    [(some x) x]
                    [(none) '()])])

        (struct-copy trie-node trie
            [label (some (append prefix-label trie-label))])))

(define (collect-end-of-words 
        [trie : Trie] 
        [path : (Listof (Listof Char))]
        [acc : (Listof (Listof Char))]) : (Listof (Listof Char))
    (let ([my-label 
        (match (trie-node-label trie)
            [(some x) x]
            [(none) '()])])
            (when print? (printf "my-label: ~v, eow? ~v~n" my-label (trie-node-end-of-word trie)))
        (if (trie-node-end-of-word trie)
            (begin (when print? (printf "~ntri-eow ~v~n~n" trie))
            (cons (cons my-label path) acc)
            )

            (let ([path (cons my-label path)])
                (when print? (printf "~ntri-path: ~v~n~n" path))
                (for/fold ([acc : (Listof (Listof Char)) acc]) ([a : Trie (trie-node-children trie)])
                    (let ([child-label 
                            (match (trie-node-label a)
                                [(some x) x]
                                [(none) '()])])
                        (if (trie-node-end-of-word a)
                            (begin (when print? (printf "~ntri-child eow: ~v~n" a))
                            (cons (cons child-label path) acc)
                            )
                            (collect-end-of-words a path acc))))))))

(define (flatten-trie
    [trie : Trie]) : (Listof Trie)

    (if (trie-node-end-of-word trie)
        ;this is a full word, don't go deeper
        (cons trie empty)

        (let ([children 
            (for/fold ([acc : (Listof Trie) '()]) ([child : Trie (trie-node-children trie)])
                (if (trie-node-end-of-word child)
                    ;need to add the prefix from parent
                    (cons (add-prefix (trie-node-label trie) child) acc)
                    (let ([subtrie : (Listof Trie) (flatten-trie child)])
                        (for/fold ([acc : (Listof Trie) acc]) ([c subtrie])
                            (cons c acc)))))])
            children)))

(define (autocomplete 
    [characters : (Listof Char)]) : (Maybe (Listof (Listof Char)))

    (define (traverse 
        [breadcrumbs : (Listof FindResult)] 
        [trie : Trie]) : Trie
        (when print? (printf "~v                    ~nEOW? ~v~n" trie (trie-node-end-of-word trie)))
        (match breadcrumbs
            [(cons (update-child index) remaining)
                (traverse remaining (list-ref (trie-node-children trie) index))]
            [(cons (exists index) remaining)
                (if (null? remaining)
                    trie
                    (let* ([my-label 
                            (match (trie-node-label trie)
                                [(some x) x]
                                [(none) '()])]
                        [child (traverse remaining (list-ref (trie-node-children trie) index))]
                        [child-label (match (trie-node-label child) [(some x) x] [(none) '()])])
                        (trie-node 
                            (some (append my-label child-label)) 
                            (trie-node-children child) 
                            (trie-node-end-of-word child)
                            )))]
            
            [(cons (replace-node _ common rest-id rest-child) _)
                (if (null? rest-id)
                    (trie-node (some rest-child) (trie-node-children trie) #t)
                    (trie-node (some common) (trie-node-children trie) #f))]
            [(cons (add-leaf _ rest-id) _ )
                (when print? (printf "ADDLEAF FOUND~n"))

                (trie-node (some '()) '() #t)]

            [_ trie]))
    (if (null? characters) 
        (none)
        (let ([breadcrumbs (find characters known-identifiers 0)])
            (when print? (printf "~n~n~v      ~v~n~n" characters breadcrumbs))
            (match breadcrumbs 
                [(none) (none)]
                [(some (cons (replace-node _ _ _ _) _)) (none)]
                [(some (cons (add-leaf _ _) _)) (none)]
                [(some thing)
                    (let* ([subtrie : Trie (traverse thing known-identifiers)]
                            [completions : (Listof (Listof Char)) 
                                (for/list ([c (collect-end-of-words subtrie '() '())])
                                    (when print? (displayln (flatten (reverse c))))
                                    (suffix characters (flatten (reverse c))))])
                        ((inst some (Listof (Listof Char))) completions))]
        ))))



;TODO: consider having function keys (f1-f4) switch tabs in the completion popup window
;that can filter between types functions methods properties etc

(define (list-suffix? 
    [lst : (Listof Char)] 
    [potential-suffix : (Listof Char)]) : Boolean
    (let ([len (length potential-suffix)])
        (if (>= (length lst) len)
            (equal? (take-right lst len) potential-suffix)
            #f)))

(define (overlap characters end)
    (define (recurse x)
        (match x
            [(cons head tail)
                (let-values ([(common rest-characters rest-end) (split-common-prefix tail end)])
                    (if (or (null? common) (not (null? rest-characters)))
                        (recurse tail)
                        rest-end))]

            [_ '()]))

    (let-values ([(common rest-characters rest-end) (split-common-prefix characters end)])
        (if (or (null? common) (not (null? rest-characters)))
            (recurse (rest characters))
            rest-end)))


(define (suffix characters end) 
    (if (equal? characters end)
        '()
        (let ([result (overlap characters end)])
            (if (null? result)
                end
                result))))



;(set! print? #t)
