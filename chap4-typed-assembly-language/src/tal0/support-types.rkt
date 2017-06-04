#lang typed/racket

(provide (all-defined-out))

(define-type (Optional A) (U (Some A) None))
(struct {A} Some ([v : A]) #:transparent)
(struct None () #:transparent)
