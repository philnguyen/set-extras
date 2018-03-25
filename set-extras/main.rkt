#lang typed/racket/base

;; This module provides abbreviations and extra tools for dealing with sets
(provide ℘ ∅ ∅eq mk-∅ mk-∅eq ∪ ∩ ∋ ∈ ⊆
         set-add*
         define-set
         set-partition set-partition-to-lists
         for/union for*/union
         for/unioneq for*/unioneq
         set->predicate
         map/set
         set-add/compact
         set-intersect/differences)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/match
         racket/set)

(define-type ℘ Setof)
(define ∅ : (℘ Nothing) (set))
(define ∅eq : (℘ Nothing) (seteq))
(define ∪ set-union)
(define ∩ set-intersect)
(define mk-∅ : (→ (℘ Nothing)) (λ () ∅))
(define mk-∅eq : (→ (℘ Nothing)) (λ () ∅eq))
(define ∋ set-member?)
(define #:∀ (X) (∈ [x : X] [xs : (℘ X)]) : Boolean (∋ xs x))
(define ⊆ subset?)

(: set-add* : (∀ (A) (℘ A) (Listof A) → (℘ A)))
;; Add each element in given list to set
(define (set-add* xs x-list)
  (for/fold ([xs* : (℘ A) xs]) ([x (in-list x-list)])
    (set-add xs* x)))

;; Define set with shortened syntax for (imperative) adding and membership testing
(define-syntax define-set
  (syntax-parser
    [(_ s:id (~literal :) τ
        (~optional (~seq #:eq? use-eq?) #:defaults ([(use-eq? 0) #'#f]))
        (~optional (~seq #:as-mutable-hash? mut?:boolean) #:defaults ([(mut? 0) #'#f])))
     (with-syntax ([s-has? (format-id #'s "~a-has?" #'s)]
                   [s-add! (format-id #'s "~a-add!" #'s)]
                   [s-add*! (format-id #'s "~a-add*!" #'s)]
                   [s-union! (format-id #'s "~a-union!" #'s)]
                   [in-s (format-id #'s "in-~a" #'s)])
       (cond
         [(syntax-e #'mut?)
          #'(begin (define s : (HashTable τ True) (if use-eq? (make-hasheq) (make-hash)))
                   (define (s-has? [x : τ]) (hash-has-key? s x))
                   (define (s-add! [x : τ]) (hash-set! s x #t))
                   (define (s-add*! [xs : (Listof τ)])
                     (for-each s-add! xs))
                   (define (s-union! [xs : (℘ τ)])
                     (set-for-each xs s-add!))
                   (define-syntax-rule (in-s) (in-hash-keys s)))]
         [else
          #'(begin (define s : (℘ τ) (if use-eq? ∅eq ∅))
                   (define (s-has? [x : τ]) (∋ s x))
                   (define (s-add! [x : τ]) (set! s (set-add s x)))
                   (define (s-add*! [xs : (Listof τ)])
                     (set! s (set-add* s xs)))
                   (define (s-union! [xs : (℘ τ)]) (set! s (∪ s xs)))
                   (define-syntax-rule (in-s) (in-set s)))]))]))

(: set-partition (∀ (X) (X → Boolean) (℘ X) → (Values (℘ X) (℘ X))))
;; Partition set members into those that satisfy the predicate and the rest
(define (set-partition p xs)
  (define s∅ (if (set-eq? xs) ∅eq ∅))
  (for/fold ([pass : (℘ X) s∅] [fail : (℘ X) s∅])
            ([x (in-set xs)])
    (cond [(p x) (values (set-add pass x) fail)]
          [else (values pass (set-add fail x))])))

(: set-partition-to-lists (∀ (X) ((X → Boolean) (℘ X) → (Values (Listof X) (Listof X)))))
;; Partition set members into those that satisfy the predicate and the rest
(define (set-partition-to-lists p xs)
  (for/fold ([pass : (Listof X) '()] [fail : (Listof X) '()])
            ([x (in-set xs)])
    (cond [(p x) (values (cons x pass) fail)]
          [else (values pass (cons x fail))])))

(define-syntax for/union
  (syntax-rules (:)
    [(_ : τ (for-clauses ...) body ...)
     (for/fold ([acc : τ ∅]) (for-clauses ...)
       (∪ acc (let () body ...)))]))

(define-syntax for*/union
  (syntax-rules (:)
    [(_ : τ (for-clauses ...) body ...)
     (for*/fold ([acc : τ ∅]) (for-clauses ...)
       (∪ acc (let () body ...)))]))

(define-syntax for/unioneq
  (syntax-rules (:)
    [(_ : τ (for-clauses ...) body ...)
     (for/fold ([acc : τ ∅eq]) (for-clauses ...)
       (∪ acc (let () body ...)))]))

(define-syntax for*/unioneq
  (syntax-rules (:)
    [(_ : τ (for-clauses ...) body ...)
     (for*/fold ([acc : τ ∅eq]) (for-clauses ...)
       (∪ acc (let () body ...)))]))

(: set->predicate (∀ (X) (℘ X) → (X → Boolean)))
;; Convert set to predicate
(define ((set->predicate xs) x) (∋ xs x))

(: map/set (∀ (X Y) (X → Y) (℘ X) → (℘ Y)))
;; Like `map`, but for set
(define (map/set f xs)
  (for/set: : (℘ Y) ([x : X (in-set xs)]) (f x)))

(: set-add/compact (∀ (X) X (X X → (Option X)) → (℘ X) → (℘ X)))
(define ((set-add/compact x ?join) xs)
  (define-set subsumed-olds : X)
  (define x* : X x)
  (define do-nothing? : Boolean #f)
  (for ([xᵢ (in-set xs)] #:break do-nothing?)
    (define ?x* (?join x xᵢ))
    (when ?x*
      (cond [(eq? ?x* xᵢ) (set! do-nothing? #t)]
            [(eq? ?x* x ) (subsumed-olds-add! xᵢ)]
            [else (subsumed-olds-add! xᵢ)
                  (set! x* ?x*)])))
  (cond [do-nothing? xs]
        [else (set-add (set-subtract xs subsumed-olds) x*)]))

(: set-intersect/differences (∀ (X) (℘ X) (℘ X) → (Values (℘ X) (℘ X) (℘ X))))
;; Partition elements in 2 sets into 3: the intersection, and those unique to each
(define (set-intersect/differences xs ys)
  (define same (∩ xs ys))
  (values same (set-subtract xs same) (set-subtract ys same)))


;; Using these can be less verbose than for/or in TR with annotations sometimes

(module cheat racket/base
  (require racket/set)
  (provide (all-defined-out))
  (define (set-filter p? xs)
    (for/fold ([acc xs]) ([x (in-set xs)] #:unless (p? x))
      (set-remove acc x))))

(require typed/racket/unsafe)
(unsafe-require/typed/provide
 'cheat
 [set-filter (∀ (a b) (case->
                       [(a → Any : #:+ b) (Setof a) → (Setof b)]
                       [(a → Any) (Setof a) → (Setof a)]))])
(provide set-andmap set-ormap)

(: set-ormap (∀ (a b) (a → b) (Setof a) → (Option b)))
(define (set-ormap p xs)
  (for/or ([x (in-set xs)])
    (p x)))

(: set-andmap (∀ (a b) (a → b) (Setof a) → (U Boolean b)))
(define (set-andmap p xs) ; can't use `for/and`
  (for/fold ([res : (U Boolean b) #t]) ([x (in-set xs)])
    (and res (p x))))
