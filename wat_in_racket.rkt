#lang racket
(require "wat/ast.rkt" "wat/printer.rkt")

(wat-display (Module 
    (list 
        (Start 'funcname1)
        (Export 'testfunc 
        (ExportFuncSignature 'funcname1))
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
                (Inst 'someinstructionwithnoargs '())
                (Inst 'i64.neg (list (Const 42)))
                (Inst 'i64.add (list (Const 32) (Const 69)))
                (Inst 'if (list (Const 1) (Name 'asdf) (Const 5)))))))))
