#lang racket
(require "ast.rkt" "../wat/ast.rkt" "types.rkt")
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
        [(Int n) (Const (imm->bits n))]
        [(Bool b) (Const (imm->bits b))]
        [(Prim1 p e) (compile-prim1 p e)]
        [(If e1 e2 e3) (compile-if e1 e2 e3)]
    )
)

(define (compile-prim1 p e)
    (match p
        ['add1 (Inst 'i64.add (seq (compile-e e) (Const (imm->bits 1))))]
        ['sub1 (Inst 'i64.sub (seq (compile-e e) (Const (imm->bits 1))))]
    )
)

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
    (WatIf
        (Inst 'i64.ne (seq 
            (compile-e e1)
            (Const val-false)))
        (compile-e e2)
        (compile-e e3)
    )
)
