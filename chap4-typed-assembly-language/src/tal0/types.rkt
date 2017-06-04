#lang typed/racket

(require "support-types.rkt")
(provide (all-defined-out) (all-from-out "support-types.rkt"))

(define-type Value (U Imm Label Reg))
(struct Reg ([idx : Integer]) #:transparent)
(struct Imm ([v : Integer]) #:transparent)
(struct Label ([v : String]) #:transparent)

(define-type InsSeq (U (cons Jmp Null) (cons Ins InsSeq)))
(define-type Ins (U Assign Add IfJmp))
(struct Assign ([rd : Reg] [sv : Value]) #:transparent)
(struct Add ([rd : Reg] [sv1 : Reg] [sv2 : Value]) #:transparent)
(struct IfJmp ([cnd : Reg] [addr : Value]) #:transparent)
(struct Jmp ([v : Value]) #:transparent)

;; syntax
(define-type RegFiles (HashTable Integer Value))
(define-type-alias HeapValue InsSeq)
(define-type Heaps (HashTable String HeapValue))

(struct MachineStatus ([h : Heaps] [r : RegFiles] [i : HeapValue]) #:transparent)

