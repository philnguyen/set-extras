#lang typed/racket/base

(require racket/set
         typed/rackunit
         "main.rkt")

(define s1 (set 1 2 3 4 5))
(define s2 (set 3 4 5 6 7))
(define-values (s1s2 s1* s2*) (set-intersect/differences s1 s2))
(check-equal? s1s2 (set 3 4 5))
(check-equal? s1* (set 1 2))
(check-equal? s2* (set 6 7))

(check-equal? (map/set string-length (set "foo" "bar" "qux")) (set 3))

(let ([small-even? (set->predicate (set 0 2 4 6))])
  (check-true (small-even? 2))
  (check-false (small-even? 39)))

(define-values (evens odds) (set-partition even? (set 0 1 2 3 4 5 6 7 8 9)))
(check-equal? evens (set 0 2 4 6 8))
(check-equal? odds  (set 1 3 5 7 9))

(check-equal? (for/union : (℘ Integer) ([n '(1 2 3 4)])
                {set n (* 2 n)})
              {set 1 2 3 4 2 4 6 8})

(check-true (set-ormap integer? (set 1 2 3 'b 'c)))
(check-false (set-ormap string? (set 1 2 3 'b 'c)))
(check-true (set-andmap integer? (set 1 2 3 4 5)))
(check-false (set-andmap integer? (set 1 2 3 'b)))
(check-equal? (set-filter integer? (set 1 2 3 'b 'c)) (set 1 2 3))
