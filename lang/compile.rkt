#lang racket
(require "ast.rkt" "../wat/ast.rkt" "types.rkt")
(provide compile)
(define heap-name (gensym 'heap))
(define (compile e)
        (Module (list (Export 'main (ExportFuncSignature 'main))
                      (MemoryExport)
                      (Global heap-name (i32) (Const 0))
                      (Func (FuncSignature 'main '() (Result (i64))) '() 
                        (Body (seq (compile-e e)))))))

(define (compile-e e)
    (match e
        [(Int n) (Const (imm->bits n))]
        [(Bool b) (Const (imm->bits b))]
        [(Char c) (Const (imm->bits c))]
        [(Prim1 p e) (compile-prim1 p e)]
        [(If e1 e2 e3) (compile-if e1 e2 e3)]))

(define (compile-prim1 p e)
    (match p
        ['add1 (Add (compile-e e) (Const (imm->bits 1)))]
        ['sub1 (Sub (compile-e e) (Const (imm->bits 1)))] 
        ['zero?
            (WatIf (Eqz (compile-e e))
                (Const val-true)
                (Const val-false))]
        ['char? (compile-is-type mask-char type-char e)]
        ['char->integer
            (Sal (Sar (compile-e e) (Const char-shift)) (Const int-shift))]
        ['integer->char
            (Xor (Sal (Sar (compile-e e) (Const int-shift)) (Const char-shift)) (Const type-char))]
        ['box (store-in-heap e type-box)]
        ['unbox 'err]
        ['box? (compile-is-type ptr-mask type-box)]
        ))


;; Helper function for storing a value on the heap and pushing the masked pointer to it onto the stack.
(define (store-in-heap e type)
    (list 
    (StoreHeap (i64) (GetGlobal (Name heap-name)) (compile-e e))
    (Xor (32->64 (GetGlobal (Name heap-name))) (Const type))
    (SetGlobal (Name heap-name) (AddT (i32) (GetGlobal (Name heap-name)) (ConstT (i32) 8))))
)

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
    (WatIf (Ne (compile-e e1) (Const val-false))
        (compile-e e2)
        (compile-e e3)
    )
)

(define (compile-is-type mask type e)
    (WatIf (Eqz (Xor (And (compile-e e) (Const mask)) (Const type)))
        (Const val-true)
        (Const val-false)
    )
)
