#lang typed/racket

(require "interpreter.rkt")

(: r RegFiles)
(define r (hash 1 (Imm 2) 2 (Imm 2) 3 (Imm 4) 4 (Label "exit")))

(: h Heaps)
(define h (hash
            "prod"
            (list
              (Assign (Reg 3) (Imm 0))
              (Jmp (Label "loop")))
            "loop"
            (list
              (IfJmp (Reg 1) (Label "done"))
              (Add (Reg 3) (Reg 2) (Reg 3))
              (Add (Reg 1) (Reg 1) (Imm -1))
              (Jmp (Label "loop")))
            "done"
            (list 
              (Jmp (Reg 4)))))

(eval (MachineStatus h r (list (Jmp (Label "prod")))))
