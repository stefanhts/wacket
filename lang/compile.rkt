#lang racket
(require "ast.rkt" "../wat/ast.rkt")
(provide compile)
(define (compile e)
        (Module
            (list
                (Export 'main (ExportFuncSignature 'main))
                (Func (FuncSignature 'main '() (Result (i64))) '() (Body (list (compile-e e))))
            ) 
        
    )
)

(define (compile-e e)
    (match e
        [(Int n) (Const n)]
        [(Prim1 p e) (compile-prim1 p e)])
)

(define (compile-prim1 p e)
    [match p
        ('add1 (BiInst 'i64.add (compile-e e) (Const 1)))
        ('sub1 (BiInst 'i64.sub (compile-e e) (Const 1))) 
    ]
)