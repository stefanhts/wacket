#lang racket
(require "ast.rkt" "../wat/ast.rkt")
(provide compile)
(define (compile e)
        (Module
            (list
                (Export 'main (ExportFuncSignature 'main))
                (Func (FuncSignature 'main '() (Result (i64))) '() (Body (compile-e e)))
            ) 
        
    )
)

(define (compile-e e)
    (match e
        [(Int n) (list (Const n))])
)