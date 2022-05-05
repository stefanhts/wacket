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

(define (compile-e e c)
    (match e
        [(Var v) (compile-var v c)]
        [(Int n) (Const (imm->bits n))]
        [(Bool b) (Const (imm->bits b))]
        [(Char c) (Const (imm->bits c))]
        [(Prim1 p e) (compile-prim1 p e)]
        [(Prim2 p e1 e2) (compile-prim2 p e1 e2)]
        [(If e1 e2 e3) (compile-if e1 e2 e3)]))

(define (compile-prim1 p e c)
    (match p
        ['add1 (Add (compile-e e c) (Const (imm->bits 1)))]
        ['sub1 (Sub (compile-e e c) (Const (imm->bits 1)))] 
        ['zero?

            (WatIf (Eqz (compile-e e))
                (Const val-true)
                (Const val-false))]
        ['char? (compile-is-type mask-char type-char e)]
        ['char->integer
            (Sal (Sar (compile-e e c) (Const char-shift)) (Const int-shift))]
        ['integer->char
            (Xor (Sal (Sar (compile-e e) (Const int-shift)) (Const char-shift)) (Const type-char))]
        ['box (store-box e)]
        ['unbox (load-from-heap e type-box (Const 0))]
        ['box? (compile-is-type ptr-mask type-box e)]
        ['car (load-from-heap e type-cons (Const 8))]
        ['cdr (load-from-heap e type-cons (Const 0))]
        ['cons? (compile-is-type ptr-mask type-cons e)]
))
(define (compile-prim2 p e1 e2 c)
   (let ((e1 (compile-e e1 c)) (e2 (compile-e e2 c)))
        (match p
            ['eq? (Eq e1 e2)]
            ['< (Lt e1 e2)]
            ['> (Gt e1 e2)]
            ['>= (Ge e1 e2)]
            ['<= (Le e1 e2)]
            ['add (Add e1 e2)]
            ['sub (Sub e1 e2)]
            ['mult (Mult e1 e2)]
            ['div (Div e1 e2)]
            ['or (Or e1 e2)]
            ['and (And e1 e2)]
            ['xor (Xor e1 e2)]
            ['>> (Sar e1 e2)]
            ['<< (Sal e1 e2)]
            ['cons (seq
            (get-tagged-heap-address type-cons) ; The return value.
            (GetGlobal (Name heap-name))        ; The first cell of the cons.
            (increment-heap-pointer)
            (GetGlobal (Name heap-name))        ; The second cell of the cons.
            (increment-heap-pointer)
            (StoreHeap (i64) (compile-e e1))
            (StoreHeap (i64) (compile-e e2))
        )]
        )
   ) 
)

;; Increments the heap pointer to the next available position.
(define (increment-heap-pointer)
    (seq (SetGlobal (Name heap-name) (AddT (i32) (GetGlobal (Name heap-name)) (ConstT (i32) 8)))))

;; Returns the current address pointed to on the heap, tagged with the given type.
(define (get-tagged-heap-address type)
    (seq (Xor (32->64 (GetGlobal (Name heap-name))) (Const type)))
)

;; Stores a box on the heap.
(define (store-box e)
    (seq
        (get-tagged-heap-address type-box)
        (GetGlobal (Name heap-name))
        (increment-heap-pointer)
        (StoreHeap (i64) (compile-e e))))

;; Helper function for getting a value from the heap and pushing it's value to the stack.
(define (load-from-heap e type offset)
    (seq
        (LoadHeap (i64) (64->32 (Add offset (Xor (Const type) (compile-e e)))))
    ))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3 c)
    (WatIf (Ne (compile-e e1 c) (Const val-false c))
        (compile-e e2 c)
        (compile-e e3 c)
    )
)

(define (compile-is-type mask type e)
    (WatIf (Eqz (Xor (And (compile-e e) (Const mask)) (Const type)))
        (Const val-true)
        (Const val-false)
    )
)
