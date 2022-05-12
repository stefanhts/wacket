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
            (Module (list (Import 'io 'read (FuncSignature 'readByte '() (Result (i64))))
                          (Import 'io 'write (FuncSignature 'writeByte (list '_) (Result (i64))))
                          (Import 'io 'peek (FuncSignature 'peekByte '() (Result (i64))))
                          (Import 'err 'error (FuncSignature 'error '() (Result (i64))))
                          (Export 'main (ExportFuncSignature 'main))
                          (MemoryExport)
                          (Global heap-name (i32) (Const 0))
                          (Global stack-name (i32) (Const  top-stack-address))
                          (Func (FuncSignature 'main '() (Result (i64))) '(assert_scratch) 
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
                '(assert_scratch)
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
        [(Eof) (Const (imm->bits eof))]
        [(Prim0 p) (compile-prim0 p)]
        [(Prim1 p e) (compile-prim1 p e c)]
        [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
        [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
        [(Begin e1 e2) (compile-begin e1 e2 c)]
        [(Var id) (compile-variable id c)]
        [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
        [(Let id e1 e2) (compile-let id e1 e2 c)]
        [(App f es) (seq (compile-app f es c))]))

(define (compile-variable id c)
    (match (lookup id c)
        ['param (seq (GetLocal (Name id)))] 
        [i (seq 
            (LoadHeap (i64) (SubT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) (+ i 8)))))]))

;; does not recursively compile
(define (runtime-bits->int e)
    (Sar e (Const int-shift)))

;; does not recursively compile
(define (runtime-int->bits e)
    (Sal e (Const int-shift)))
            
(define (compile-prim0 p)
    (match p
        ['void (Const val-void)]
        ['read-byte (Call 'readByte)]
        ['peek-byte (Call 'peekByte)]))

(define (compile-prim1 p e c)
    (match p
        ['add1 (Add (assert-integer (compile-e e c)) (Const (imm->bits 1)))]
        ['sub1 (Sub (assert-integer (compile-e e c)) (Const (imm->bits 1)))] 
        ['zero?
            (WatIf (Eqz (assert-integer (compile-e e c)))
                (Const val-true)
                (Const val-false))]
        ['char? (compile-is-type mask-char type-char e c)]
        ['char->integer
            (Sal (Sar (assert-char (compile-e e c)) (Const char-shift)) (Const int-shift))]
        ['integer->char
            (Xor (Sal (Sar (assert-codepoint (compile-e e c)) (Const int-shift)) (Const char-shift)) (Const type-char))]
        ['eof-object? 
            (WatIf (Eq (compile-e e c) (Const val-eof))
                (Const val-true)
                (Const val-false))]
        ['write-byte (seq (assert-byte (compile-e e c)) (Call 'writeByte))]
        ;; TODO: assert box, etc
        ['box (store-box e c)]
        ['unbox (load-from-heap e type-box (Const 0) c)]
        ['box? (compile-is-type ptr-mask type-box e c)]
        ['car (load-from-heap e type-cons (Const 8) c)]
        ['cdr (load-from-heap e type-cons (Const 0) c)]
        ['cons? (compile-is-type ptr-mask type-cons e c)]))
        
(define (compile-prim2 p e1 e2 c)
   (let ((e1 (compile-e e1 c)) (e2 (compile-e e2 c)))
        (match p
            ['eq? (Eq e1 e2)]
            ['< (Lt (assert-integer e1) (assert-integer e2))]
            ['> (Gt (assert-integer e1) (assert-integer e2))]
            ['>= (Ge (assert-integer e1) (assert-integer e2))]
            ['<= (Le (assert-integer e1) (assert-integer e2))]
            ['+ (Add (assert-integer e1) (assert-integer e2))]
            ['- (Sub (assert-integer e1) (assert-integer e2))]
            ['* (runtime-int->bits (Mul (runtime-bits->int (assert-integer e1)) (runtime-bits->int(assert-integer e2))))]
            ['/ (runtime-int->bits (Div (runtime-bits->int (assert-integer e1)) (runtime-bits->int(assert-integer e2))))]
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

(define (compile-begin e1 e2 c)
    (seq (compile-e e1 c)
         (Drop)
         (compile-e e2 c)))

;; Stores a box on the heap.
(define (store-box e c)
    (seq
        (get-tagged-heap-address type-box)
        (GetGlobal (Name heap-name))
        (increment-heap-pointer)
        (StoreHeap (i64) (compile-e e c))))

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
    ['() (error (string-append (symbol->string x) ": Symbol does not exist in this scope"))]
    [(cons (cons type y) rest)
     (match (eq? x y)
       [#t (match x
            [local 0]
            [param param])]
       [#f (match x
            [local (+ 8 (lookup x rest))]
            [param (lookup x rest)]
       )])])) 
       
(define (err)
    (Call 'error)
)

(define (assert-type mask type)
    (λ (arg)
        (seq (SetLocal (Name 'assert_scratch) arg)
             (WatIf (Ne (And (GetLocal (Name 'assert_scratch)) mask) type) 
                    (err) 
                    (GetLocal (Name 'assert_scratch))))))

(define assert-integer
    (assert-type (Const mask-int) (Const type-int)))
(define assert-char
    (assert-type (Const mask-char) (Const type-char)))

(define (assert-codepoint arg)
    (seq (SetLocal (Name 'assert_scratch) arg)
         (WatIf (Lt (GetLocal (Name 'assert_scratch)) (imm->bits 0)) 
                (err)
                (WatIf  (Gt (GetLocal (Name 'assert_scratch)) (imm->bits 1114111)) 
                        (err)
                        (WatIf (And (Ge (GetLocal (Name 'assert_scratch)) (imm->bits 55295)) 
                                    (Le (GetLocal (Name 'assert_scratch)) (imm->bits 57344))) 
                               (err) 
                               (GetLocal (Name 'assert_scratch)))))))

(define (assert-byte arg)
    (seq (SetLocal (Name 'assert_scratch) arg)
         (WatIf (Lt (GetLocal (Name 'assert_scratch)) (imm->bits 0)) 
                (err)
                (WatIf (Gt (GetLocal (Name 'assert_scratch)) (imm->bits 255)) 
                       (err) 
                       (GetLocal (Name 'assert_scratch))))))
