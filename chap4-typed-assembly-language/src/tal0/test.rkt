#lang typed/racket

(require "interpreter.rkt")
(require "lang-utils.rkt")

(: R RegFiles)
(define R (hash 1 (Imm 2) 2 (Imm 2) 3 (Imm 4) 4 (Label "exit")))

(: H Heaps)
(define H
  {::tal0::
  pred (
        (Assign (Reg 3) (Imm 0))
        (Jmp (Label "loop")))

  loop (
        (IfJmp (Reg 1) (Label "done"))
        (Add (Reg 3) (Reg 2) (Reg 3))
        (Add (Reg 1) (Reg 1) (Imm -1))
        (Jmp (Label "loop")))

  done (
        (Jmp (Reg 4)))
  })

(eval (MachineStatus H R (list (Jmp (Label "prod")))))
