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
        [(Prim2 p e1 e2) (compile-prim2 p e1 e2)]
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
        ['box (store-box e)]
        ['unbox (load-from-heap e type-box (Const 0))]
        ['box? (compile-is-type ptr-mask type-box e)]
        ['car (load-from-heap e type-cons (Const 8))]
        ['cdr (load-from-heap e type-cons (Const 0))]
        ['cons? (compile-is-type ptr-mask type-cons e)]
))

(define (compile-prim2 p e1 e2)
    (match p
        ['cons (seq
            ;;; (store-in-heap e2 type-cons #t)
            ;;; (store-in-heap e1 0 #f)
        )]   
))

;; Increments the heap pointer.
(define (increment-heap-pointer)
    (seq (SetGlobal (Name heap-name) (AddT (i32) (GetGlobal (Name heap-name)) (ConstT (i32) 8)))))


;; Stores a box on the heap.
(define (store-box e)
    (seq
        (Xor (32->64 (GetGlobal (Name heap-name))) (Const type-box))
        (StoreHeap (i64) (seq
            (GetGlobal (Name heap-name))
            (increment-heap-pointer))
            (compile-e e))))

;; Helper function for getting a value from the heap and pushing it's value to the stack.
(define (load-from-heap e type offset)
    (seq
        (LoadHeap (i64) (64->32 (Add offset (Xor (Const type) (compile-e e)))))
    ))

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
