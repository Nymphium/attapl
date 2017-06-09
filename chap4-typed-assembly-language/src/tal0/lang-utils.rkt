#lang typed/racket

(require "types.rkt")

(provide ::tal0::)

(define-type-alias Label-HeapValue (Pairof String InsSeq))
(define-type TalMac (Pairof Label-HeapValue TalMac))

(define-syntax (::tal0:: stx)
  (syntax-case stx ()
    [(_ e ...) #'(build-heaps (tal0-syntax e ...))]))

(define-syntax (tal0-syntax stx)
  (syntax-case stx ()
    [(_ label (ins0 ins ...))
     #'((inst list Label-HeapValue) ((inst cons String InsSeq) (symbol->string (syntax-e #'label)) (list ins0 ins ...)))]
    [(_ label (ins0 ins ...) rest ...)
     #'({inst cons Label-HeapValue TalMac} ((inst cons String InsSeq) (symbol->string (syntax-e #'label)) (list ins0 ins ...)) (tal0-syntax rest ...))]))

(: build-heaps (-> (Listof (Pairof String HeapValue)) Heaps))
(define (build-heaps p)
  (: inner (-> Heaps  (Listof (Pairof String HeapValue)) Heaps))
  (define (inner h l)
    (match l
      [(list (cons label heap)) ({inst hash-set String HeapValue} h label heap)]
      [(cons (cons label heap) rest)
       (inner ({inst hash-set String HeapValue} h label heap) rest) ]))
  (inner (make-immutable-hash) p))
