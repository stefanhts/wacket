#lang racket
(require "ast.rkt" "../wat/ast.rkt")
(provide compile)
(define (compile e)
        (Module
            (list
                (Export 'main (ExportFuncSignature 'main))
                (Func (FuncSignature 'main '() (Result (i64))) '() (Body (seq (compile-e e))))
            ) 
        
    )
)

(define (compile-e e)
    (match e
        [(Int n) (Const n)]
        [(Prim1 p e) (compile-prim1 p e)]
        [(IfZero e1 e2 e3) (compile-ifzero e1 e2 e3)]
    )
)

(define (compile-prim1 p e)
    [match p
        ('add1 (Inst 'i64.add (seq (compile-e e) (Const 1))))
        ('sub1 (Inst 'i64.sub (seq (compile-e e) (Const 1)))) 
    ]
)

;; Expr Expr Expr -> Asm
(define (compile-ifzero e1 e2 e3)
    (Inst 'if (seq (Result (i64))
        (Inst 'i64.eqz (seq (compile-e e1))) 
        (compile-e e2)
        (compile-e e3)))
)
