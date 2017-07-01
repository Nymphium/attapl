#lang typed/racket

(require "types.rkt"
         (for-syntax "types.rkt"))

(provide ::H::
         ::R::
         ::I::)

(define-type-alias Label-HeapValue (Pairof String InsSeq))
(define-type TalMac (Pairof Label-HeapValue TalMac))

(define-syntax (::H:: stx)
  (syntax-case stx ()
    [(_ e ...) #'(build-heaps (tal0-syntax e ...))]))

(define-syntax (::R:: stx)
  (syntax-case stx ()
    [(_ e ...) #'(build-regfile (reg-values e ...))]))

(define-syntax (reg-values stx)
  (syntax-case stx ()
    [(_ x) #'(list (genopr x))]
    [(_ x xs ...) #'(cons (genopr x) (reg-values xs ...))]))

(define-syntax (genopr stx)
  (syntax-case stx ()
    [(_ oprx)
     (let ([dat (syntax->datum #'oprx)])
       (if (number? dat)
         (with-syntax ([num (datum->syntax stx dat)])
           #'(Imm num))
         (let* ([str (symbol->string dat)]
                [rv (regexp-match #rx"r([0-9]+)" str)])
           (if (list? rv)
             (with-syntax ([idx (string->number (car (cdr rv)))])
               #'(Reg idx))
             (with-syntax ([lbl (datum->syntax stx str)])
               #'(Label lbl))))))]))

(define-syntax (tal0-syntax stx)
  (syntax-case stx ()
    [(_ label (ins0 ins ...))
     (with-syntax ([label~ (datum->syntax stx (symbol->string (syntax-e #'label)))])
       #'((inst list Label-HeapValue) ((inst cons String InsSeq) label~ (::I:: ins0 ins ...))))]
    [(_ label (ins0 ins ...) rest ...)
     (with-syntax ([label~ (datum->syntax stx (symbol->string (syntax-e #'label)))])
      #'((inst cons Label-HeapValue TalMac) ((inst cons String InsSeq) label~ (::I:: ins0 ins ...)) (tal0-syntax rest ...)))]))

(define-syntax (::I:: stx)
  (syntax-case stx (jump := + if)
    [(_ if v jump v2 rest ...) #'(ann (cons (IfJmp (genopr v) (genopr v2)) (::I:: rest ...)) InsSeq)]
    [(_ jump v) #'(ann (list (Jmp (genopr v))) InsSeq)]
    [(_ r1 := r2 + v rest ...) #'(ann (cons (Add (genopr r1) (genopr r2) (genopr v)) (::I:: rest ...)) InsSeq)]
    [(_ r1 := v rest ...) #'(ann (cons (Assign (genopr r1) (genopr v)) (::I:: rest ...)) InsSeq)]))

(: build-heaps (-> (Listof (Pairof String HeapValue)) Heaps))
(define (build-heaps p)
  (: inner (-> Heaps  (Listof (Pairof String HeapValue)) Heaps))
  (define (inner h l)
    (match l
      [(list (cons label heap)) (#{hash-set @ String HeapValue} h label heap)]
      [(cons (cons label heap) rest)
       (inner (#{hash-set @ String HeapValue} h label heap) rest) ]))
  (inner (make-immutable-hash) p))

(: build-regfile (-> (Listof Value) RegFiles))
(define (build-regfile inss)
  (let ([idx 0])
    (#{foldl @ Value RegFiles Any Any}
     (lambda ([x : Value] [h : RegFiles])
       (set! idx (add1 idx))
       (#{hash-set @ Integer Value} h idx x))
     (make-immutable-hash) inss)))
