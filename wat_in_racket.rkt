#lang racket
(require "wat/ast.rkt" "wat/printer.rkt")

(display (wat-out (Module 
    (list 
        (Start 'funcname1)
        (Export 'testfunc 
        (FuncSignature 
                'funcname1 
                (list 
                    (Param 'firstparam (i64))
                    (Param 'secondparam (i64))) 
                (Result (i64))))
        (Import 'mod 'func 
            (FuncSignature 
                'funcname1 
                (list 
                    (Param 'firstparam (i64))
                    (Param 'secondparam (i64))) 
                (Result (i64))) )
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
                (UnInst 'i64.neg (Const 42))
                (BiInst 'i64.add (Const 32) (Const 69)))))))))
