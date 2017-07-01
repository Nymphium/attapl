#lang typed/racket

(require "interpreter.rkt"
         "lang-utils.rkt")

(: R RegFiles)
(define R (::R:: 2 2 4 exit))

(: H Heaps)
(define H
  {::H::
  prod (
        r3 := 0
        jump loop)

  loop (
        if r1 jump done
        r3 := r2 + r3
        r1 := r1 + -1
        jump loop)

  done (jump r4)})

(eval (MachineStatus H R (::I:: jump prod)))
