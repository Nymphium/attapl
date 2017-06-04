#lang typed/racket

(require
  "types.rkt"
  racket/match)
(provide (all-defined-out) (all-from-out "types.rkt"))

(: regfiles-ref (-> RegFiles Reg (Optional Value)))
(define (regfiles-ref regf reg)
  (let ([v (#{hash-ref : (-> RegFiles Integer (-> False) (U Value False))} regf (Reg-idx reg) (lambda () #f))])
    (if v (Some v) (None))))

(: heaps-ref (-> Heaps Label (Optional HeapValue)))
(define (heaps-ref h l)
  (let ((v (#{hash-ref : (-> Heaps String (-> False) (U HeapValue False))} h (Label-v l) (lambda () #f))))
    (if v (Some v) (None))))

(: rhat (-> RegFiles Value (Optional Value)))
(define/match (rhat regf v)
  [(regf (Reg _))
   (regfiles-ref regf v)]
  [(_ v) (Some v)])

(: eval (-> MachineStatus RegFiles))
(define/match (eval m)
  [((MachineStatus h r (cons (Jmp v) null)))
   (match v
     [(Reg idx)
      (match (regfiles-ref r v)
        [(Some (Label l))
         (match (heaps-ref h (Label l))
           [(Some i) (eval (MachineStatus h r i))]
           [(None) #:when (equal? l "exit") r]
           [_ (error (format "invalid operand for Jmp ~a / heap reference ~a" v (Label l)))])]
        [_ (error (format "invalid operand for Jmp ~a / register reference ~a" v v))])]
     [(Label "exit") r]
     [(Label l)
      (match (heaps-ref h v)
        [(Some i) (eval (MachineStatus h r i))]
        [_ (error (format "invalid operand for Jmp ~a" v))])]
     [_ (error (format "invalid operand for Jmp ~a" v))])]
  [((MachineStatus h r (cons (Assign (Reg rsi) rd) i)))
   (eval (MachineStatus h (hash-set r rsi rd) i))]
  [((MachineStatus h r (cons (Add (Reg rsi) rd v) i)))
   (match* ((rhat r rd) (rhat r v))
     [((Some (Imm n)) (Some (Imm m))) (eval (MachineStatus h (hash-set r rsi (Imm (+ n m))) i))]
     [(_ _) (error (format "invalid operand for Add ~a ~a" rd v))])]
  [((MachineStatus h r (cons (IfJmp #{rs : Reg} addr) i)))
   (match (regfiles-ref r rs)
     [(Some (Imm n))
      (if (= n 0)
        (match addr
          [(Label l)
           (match (heaps-ref h addr)
             [(Some i~) (eval (MachineStatus h r i~))]
             [(None) #:when (equal? l "exit") r]
             [_ (error (format "invalid operand for IfJmp / address ~a" addr))])]
          [(Reg rd)
           (match (regfiles-ref r addr)
             [(Some (Label l))
              (match (heaps-ref h (Label l))
                [(Some i~) (eval (MachineStatus h r i~))]
                [(None) #:when (equal? l "exit") r]
                [_ (error (format "invalid operand for IfJmp / address ~a" addr))])]
             [_ (error (format "invalid operand for IfJmp / address ~a" addr))])]
          [_ (error (format "invalid operand for IfJmp / address ~a" addr))])
        (eval (MachineStatus h r i)))])])

;; misc
(: make-insseq (-> (U Null (Listof Ins)) Jmp InsSeq))
(define (make-insseq ins-list jmp)
  (match ins-list
    ['() (list jmp)]
    [(cons ins ins-list~) (cons ins (make-insseq ins-list~ jmp))]))

