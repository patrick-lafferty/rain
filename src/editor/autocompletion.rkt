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
#lang typed/racket/base

(provide autocomplete)

(require "../functional/maybe.rkt"
    racket/list
    racket/match)
    ;"../interpreter.rkt")

(define-type Trie trie-node)
(struct trie-node (
    [label : (Maybe (Listof Char))]
    [children : (Listof Trie)])
    #:transparent)   

(define known-identifiers (trie-node (some '()) '()))

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
                    [(cons x (some (list (exists y)))) (some (cons (exists x) (list (exists y))))]
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
                            ;(some (list 'exists))
                            (some (list (exists parent-index)))
                            #|(if ((inst findf Trie) (lambda (t) (none? (trie-node-label t))) (trie-node-children trie))
                                (some (list 'exists))
                                (none))|#
                            ;end of search word but label still has more letters, split label
                            (some (list (replace-node parent-index common rest-id rest-label))))
                        ;more left in search word
                        (if (null? rest-label)
                            ;more left in search word but label is a proper prefix of search word
                            (let ([result (find-children rest-id (trie-node-children trie) 0 parent-index)])
                                (match result
                                    [(cons x (some (list (exists y)))) (some (cons (exists x) (list (exists y))))]
                                    [(cons -1 (some change)) (some change)]
                                    [(cons x (some change)) (some (cons (update-child x) change))]))
                            (some (list (replace-node parent-index common rest-id rest-label)))))))]))

(define (replace 
        [trie : Trie]
        [breadcrumbs : (Maybe (Listof FindResult))]) : Trie
    (match breadcrumbs
        [(none) trie]
        [(some (list (exists _))) trie]
        [(some (cons (update-child index) remaining))
            (let ([children (trie-node-children trie)])
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

(define (insert
    [identifier : (Listof Char)]
    [trie : Trie]) : Trie
    (let ([breadcrumbs (find identifier trie 0)])
        (replace trie breadcrumbs)))

(: print-trie (->* (Trie) (Integer) Void))
(define (print-trie 
    trie [indent 0])
    (printf "~a~v~n" (make-string indent #\space) (trie-node-label trie))
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
    ))

(let ([thing

(for/fold ([trie : Trie (trie-node (some '()) empty)])
    ([id : Symbol interpreter-keywords])
    (insert (string->list (symbol->string id)) trie))])
(set! known-identifiers thing)
)

(define (flatten-trie
    [trie : Trie]) : (Listof Trie)

    (if (null? (trie-node-children trie))
        (cons trie empty)
        (cons trie 
            (for/fold ([acc : (Listof Trie) '()]) ([child : Trie (trie-node-children trie)]) 
                (let ([subtrie : (Listof Trie) (flatten-trie child)])
                    (for/fold ([acc : (Listof Trie) acc]) ([c subtrie])
                        (cons c acc)))))))
(print-trie known-identifiers)
(define (autocomplete 
    [characters : (Listof Char)]) : (Maybe (Listof (Listof Char)))

    (define (traverse 
        [breadcrumbs : (Listof FindResult)] 
        [trie : Trie]) : Trie
        (match breadcrumbs
            [(cons (update-child index) remaining)
                (traverse remaining (list-ref (trie-node-children trie) index))]
            [(cons (exists index) remaining)
                (if (null? remaining)
                    trie
                    (traverse remaining (list-ref (trie-node-children trie) index)))]

            [(cons (replace-node _ _ _ rest-child) _)
                (trie-node (some rest-child) (trie-node-children trie))]
            [_ trie]))
    (if (null? characters) 
        (none)
        (let ([breadcrumbs (find characters known-identifiers 0)])
            (match breadcrumbs 
                [(none) (none)]
                [(some (cons (replace-node _ _ _ _) _)) (none)]
                [(some (cons (add-leaf _ _) _)) (none)]
                [(some thing)
                    (let* ([subtrie : Trie (traverse thing known-identifiers)]
                            [completions : (Listof (Listof Char)) 
                                (for/fold ([acc : (Listof (Listof Char)) '()]) ([trie : Trie (flatten-trie subtrie)]);(trie-node-children subtrie)])
                                    (match (trie-node-label trie)
                                        [(some x) (cons x acc)]
                                        [(none) acc]))])
                        ((inst some (Listof (Listof Char))) completions))]
        ))))

                    
;TODO: consider having function keys (f1-f4) switch tabs in the completion popup window
;that can filter between types functions methods properties etc