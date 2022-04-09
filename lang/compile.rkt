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
        [(Prim1 p e) (compile-prim1 p e)]
        ; [(IfZero e1 e2 e3) (compile-ifzero e1 e2 e3)]
    )
)

(define (compile-prim1 p e)
    [match p
        ('add1 (Inst 'i64.add (list (compile-e e) (Const 1))))
        ('sub1 (Inst 'i64.sub (list (compile-e e) (Const 1)))) 
    ]
)

;; Expr Expr Expr -> Asm
; (define (compile-ifzero e1 e2 e3)

;   )
