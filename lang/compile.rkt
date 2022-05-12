#lang racket
(require "ast.rkt" "../wat/ast.rkt" "types.rkt")
(provide compile)
(define local 'local)
(define param 'param)
(define stack-name (gensym 'stack))
(define top-stack-address 16384)
(define heap-name (gensym 'heap))
(define (compile e)
    (match e
        [(Prog ds e)
            (Module (list (Export 'main (ExportFuncSignature 'main))
                      (MemoryExport)
                      (Global heap-name (i32) (Const 0))
                      (Global stack-name (i32) (Const  top-stack-address))
                      (Func (FuncSignature 'main '() (Result (i64))) '() 
                         (Body (seq (compile-e e '()))))
                      (FuncList (compile-defines ds))))]))

(define (compile-es es c)
    (match es
        ['() '()]
        [(cons e es)
            (seq (compile-e e c)
                 (compile-es es c))]))

(define (compile-defines ds)
    (match ds
        ['() (seq)]
        [(cons d ds)
          (seq (compile-define d)
               (compile-defines ds))]))

(define (compile-define d)
    (match d
        [(Defn f xs e)
         (seq (Func
                (FuncSignature f xs (Result (i64)))
                '()
                (Body (seq (compile-e e (params xs))))
                ))]))

(define (params xs)
    (map cons (make-list (length xs) param) (reverse xs))
)

(define (compile-app f es c)
    (seq (compile-es es c)
         (Call f)))

(define (compile-e e c)
    (match e
        [(Int n) (Const (imm->bits n))]
        [(Bool b) (Const (imm->bits b))]
        [(Char c) (Const (imm->bits c))]
        [(Var id) (compile-variable id c)]
        [(Str s)  (compile-string s)]
        [(Prim1 p e) (compile-prim1 p e c)]
        [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
        [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
        [(Let id e1 e2) (compile-let id e1 e2 c)]
        [(App f es) (seq (compile-app f es c))]))

(define (compile-variable id c)
    (match (lookup id c)
        [param (seq (GetLocal (Name id)))] 
        [i (seq 
            (LoadHeap (i64) (SubT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) (+ i 8)))))]))

(define (compile-prim1 p e c)
    (match p
        ['add1 (Add (compile-e e c) (Const (imm->bits 1)))]
        ['sub1 (Sub (compile-e e c) (Const (imm->bits 1)))] 
        ['zero?
            (WatIf (Eqz (compile-e e c))
                (Const val-true)
                (Const val-false))]
        ['char? (compile-is-type mask-char type-char e c)]
        ['char->integer
            (Sal (Sar (compile-e e c) (Const char-shift)) (Const int-shift))]
        ['integer->char
            (Xor (Sal (Sar (compile-e e c) (Const int-shift)) (Const char-shift)) (Const type-char))]
        ['box (store-box e c)]
        ['unbox (load-from-heap e type-box (Const 0) c)]
        ['box? (compile-is-type ptr-mask type-box e c)]
        ['car (load-from-heap e type-cons (Const 8) c)]
        ['cdr (load-from-heap e type-cons (Const 0) c)]
        ['cons? (compile-is-type ptr-mask type-cons e c)]
        ['string? (compile-is-type ptr-mask type-str e c)]))
        
(define (compile-prim2 p e1 e2 c)
   (let ((e1 (compile-e e1 c)) (e2 (compile-e e2 c)))
        (match p
            ['eq? (Eq e1 e2)]
            ['< (Lt e1 e2)]
            ['> (Gt e1 e2)]
            ['>= (Ge e1 e2)]
            ['<= (Le e1 e2)]
            ['+ (Add e1 e2)]
            ['- (Sub e1 e2)]
            ['+ (Mul e1 e2)]
            ['/ (Div e1 e2)]
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
            (StoreHeap (i64) e1)
            (StoreHeap (i64) e2))])))

(define (compile-let id e1 e2 c)
    (seq
        (GetGlobal (Name stack-name))
        (StoreHeap (i64) (compile-e e1 c))
        (SetGlobal (Name stack-name) (AddT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) 8)))
        (compile-e e2 (cons (cons 'local id) c))
    ))

;; Pushes the given value to the stack.
(define (push-to-stack e c) 
    (seq
        (GetGlobal (Name stack-name))
        (StoreHeap (i64) (compile-e e c))
        (SetGlobal (Name stack-name) (AddT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) 8))) ; Add pointer simulate pushing.
))

;; Pops the value at the top of the stack.
(define (pop-stack)
    (seq
        ;; TODO: Jump to error if stack value is at `top-stack-address`.
        (SetGlobal (Name stack-name) (SubT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) 8))) ; Decrement pointer simulate popping.
        (LoadHeap (i64) (GetGlobal (Name stack-name))) ; Retrieve the value here.
))

;; Increments the heap pointer to the next available position.
(define (increment-heap-pointer)
    (seq (SetGlobal (Name heap-name) (AddT (i32) (GetGlobal (Name heap-name)) (ConstT (i32) 8)))))

;; Returns the current address pointed to on the heap, tagged with the given type.
(define (get-tagged-heap-address type)
    (seq (Xor (32->64 (GetGlobal (Name heap-name))) (Const type)))
)

;; Stores a box on the heap.
(define (store-box e c)
    (seq
        (get-tagged-heap-address type-box)
        (GetGlobal (Name heap-name))
        (increment-heap-pointer)
        (StoreHeap (i64) (compile-e e c))))


(define (compile-string s)
 (let ((len (string-length s)))
    (if (zero? len) (seq (Const type-str)) 'err)
)) ; TODO(peter)

;; Helper function for getting a value from the heap and pushing it's value to the stack.
(define (load-from-heap e type offset c)
    (seq
        (LoadHeap (i64) (64->32 (Add offset (Xor (Const type) (compile-e e c)))))))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3 c)
    (WatIf (Ne (compile-e e1 c) (Const val-false))
        (compile-e e2 c)
        (compile-e e3 c)))

(define (compile-is-type mask type e c)
    (WatIf (Eqz (Xor (And (compile-e e c) (Const mask)) (Const type)))
        (Const val-true)
        (Const val-false)))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() 'err]
    [(cons (cons type y) rest)
     (match (eq? x y)
       [#t (match x
            [local 0]
            [param param])]
       [#f (match x
            [local (+ 8 (lookup x rest))]
            [param (lookup x rest)]
       )])])) 
       
