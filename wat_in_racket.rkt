#lang racket
(require "wat/ast.rkt")
(Module 
    (list 
        (Func 
            (FuncSignature 
                'funcname1 
                (list 
                    (Param 'firstparam (i64))
                    (Param 'secondparam (i64))) 
                (Result (i64))) 
            (list
                (Local 'firstlocal (i64))) 
            (Body (list
                (ZrInst 'someinstructionwithnoargs)
                (UnInst 'i64.const 42)
                (BiInst 'i64.add (UnInst 'i64.const 84) (UnInst 'i64.const 69)))))))