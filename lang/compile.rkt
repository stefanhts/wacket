#lang racket
(require "ast.rkt" "../wat/ast.rkt" "types.rkt" "fv.rkt" "lambdas.rkt")
(provide compile)
(define local 'local)
(define param 'param)
(define assert_scratch (gensym 'assert_scratch))
(define app_scratch (gensym 'app_scratch))
(define stack-name (gensym 'stack))
(define top-stack-address 16384)
(define heap-name (gensym 'heap))
(define internal-pointer (gensym 'i))
(define secondary-pointer (gensym 'j))
(define function-names '())
(define (compile p)
    (match p
        [(Prog ds e) (let* ((lams (lambdas p)) (all-funcs (append ds lams)))
            (begin (set! function-names (funcnames lams))
            (Module (list (Import 'io 'read (FuncSignature 'readByte '() (Result (i64))))
                          (Import 'io 'write (FuncSignature 'writeByte (list '_) (Result (i64))))
                          (Import 'io 'peek (FuncSignature 'peekByte '() (Result (i64))))
                          (Import 'err 'error (FuncSignature 'error '() (Result (i64))))
                          (Export 'main (ExportFuncSignature 'main))
                          (Table (length lams))
                          (Elem function-names)
                          (TypeDec)
                          (MemoryExport)
                          (Global heap-name (i32) (Const 0))
                          (Global stack-name (i32) (Const top-stack-address))
                          (Func (FuncSignature 'main '() (Result (i64))) (list assert_scratch app_scratch internal-pointer secondary-pointer)
                              (Body (seq (compile-e e (reverse (define-ids ds))))))
                          (FuncList (seq (compile-defines ds) (compile-lambda-defines lams)))))))]))

(define (compile-es es c)
    (match es
        ['() '()]
        [(cons e es)
            (seq (compile-e e c)
                 (compile-es es c))]))

(define (funcnames fs)
    (match fs
        [(cons (Lam f _ _) fs) (cons f (funcnames fs))]
        [(cons (Defn f _ _) fs) (cons f (funcnames fs))]
        ['() '()]))

;; [Listof Defn] -> [Listof Id]
(define (define-ids ds)
  (match ds
    ['() '()]
    [(cons (Defn f xs e) ds)
     (cons f (define-ids ds))]))

(define (compile-defines ds)
    (match ds
        ['() (seq)]
        [(cons d ds)
          (seq (compile-define d)
               (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
    (match d
        [(Defn f xs e)
         (seq (Func
                (FuncSignature f xs (Result (i64)))
                (list assert_scratch app_scratch internal-pointer secondary-pointer)
                (Body (seq (compile-e e (tupleize 'param xs))))
                ))]))

;; [Listof Lam] -> Asm
(define (compile-lambda-defines ls)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-lambda-define l)
          (compile-lambda-defines ls))]))

;; Lam -> Asm
(define (compile-lambda-define l)
  (let ((fvs (fv l)))
    (match l
      [(Lam f xs e)
       (let ((env (append (tupleize 'local fvs) (tupleize 'param xs))))
       (seq (Func
                (FuncSignature f xs (Result (i64)))
                (list assert_scratch app_scratch)
                (Body (seq  (Comment "copying environment to stack")
                            (copy-env-to-stack fvs 8)
                            (Comment "compililng function expression")
                            (compile-e e env)
                            (Comment "decrementing stack pointer to pop closure")
                            (SetGlobal (Name stack-name) ;; decrement stack pointer to pop closure
                                       (SubT (i32) 
                                             (GetGlobal (Name stack-name)) 
                                             (ConstT (i32) (* 8 (length fvs))))))))))])))

(define (tupleize type xs)
    (map cons (make-list (length xs) type) (reverse xs))
)

(define (compile-app-def f es c)
    (seq (compile-es es c)
         (Call f)))

(define (compile-app e es c)
    (seq (Comment "compiling app")
         (compile-es es c)
         (Comment "put function to be called on stack, after ensuring it's a proc")
         (TeeLocal (Name app_scratch) (assert-proc (compile-e e c))) 
         (CallIndirect (64->32 (load-from-heap-by-address type-proc 0 (GetLocal (Name app_scratch)))))
         (Comment "done compiling app")))

; ;; Expr [Listof Expr] CEnv -> Asm
; ;; The return address is placed above the arguments, so callee pops
; ;; arguments and return address is next frame
; (define (compile-app-nontail e es c)
;   (let ((r (gensym 'ret))
;         (i (* 8 (length es))))
;     (seq (Lea rax r)                              ;; put return adress on stack
;          (Push rax)
;          (compile-es (cons e es) (cons #f c))     ;; put result of first arg on stack followed by results of all args
;          (Mov rax (Offset rsp i))                 
;          (assert-proc rax)                        ;; make sure first arg is proc struct
;          (Xor rax type-proc)                      ;; get rid of proc bit tagging
;          (Mov rax (Offset rax 0))                 ;; fetch the code label
;          (Jmp rax)                                ;; make the call
;          (Label r))))

(define (compile-e e c)
    (match e
        [(Int n) (Const (imm->bits n))]
        [(Bool b) (Const (imm->bits b))]
        [(Char c) (Const (imm->bits c))]
        [(Var id) (compile-variable id c)]
        [(Str s)  (compile-string s)]
        [(Eof) (Const (imm->bits eof))]
        [(Prim0 p) (compile-prim0 p)]
        [(Prim1 p e) (compile-prim1 p e c)]
        [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
        [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
        [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
        [(Begin e1 e2) (compile-begin e1 e2 c)]
        [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
        [(Let id e1 e2) (compile-let id e1 e2 c)]
        [(AppDef f es) (compile-app-def f es c)]
        [(App e es) (seq (compile-app e es c))]
        [(Lam f xs e)       (compile-lam f xs e c)]))

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
        ['box (store-box e c)]
        ['unbox (load-from-heap (assert-box (compile-e e c)) type-box (Const 0))]
        ['box? (compile-is-type ptr-mask type-box e c)]
        ['car (load-from-heap (assert-cons (compile-e e c)) type-cons (Const 8))]
        ['cdr (load-from-heap (assert-cons (compile-e e c)) type-cons (Const 0))]
        ['cons? (compile-is-type ptr-mask type-cons e c)]
        ['vector? (compile-is-type ptr-mask type-vect e c)]
        ['vector-length (Sal (load-from-heap (assert-vector (compile-e e c)) type-vect (Const 0)) (Const int-shift))]
        ['string? (compile-is-type ptr-mask type-str e c)]
        ['string-length (Sal (load-from-heap (assert-string (compile-e e c)) type-str (Const 0)) (Const int-shift))]))
        
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
            ['cons (compile-cons e1 e2)]
            ['make-vector (compile-make-heap-vector e1 e2 type-vect)]
            ['vector-ref (compile-ref (assert-vector e1) e2 type-vect)]
            ['make-string (compile-make-heap-vector e1 e2 type-str)]
            ['string-ref (compile-ref (assert-string e1) e2 type-str)])))

(define (compile-prim3 p e1 e2 e3 c)
    (let ((e1 (compile-e e1 c)) (e2 (compile-e e2 c)) (e3 (compile-e e3 c)))
        (match p
            ['vector-set! (seq
                (SetLocal (Name secondary-pointer) e1)
                (SetLocal (Name 'assert_scratch) (load-from-heap (GetLocal (Name secondary-pointer)) type-vect (Const 0)))
                (WatIf (64->32 (And 
                    (32->64 (Ge (Sar e2 (Const int-shift)) (Const 0))) 
                    (32->64 (Lt (Sar e2 (Const int-shift)) (GetLocal (Name 'assert_scratch))))))

                    (seq
                        (64->32 (Add (Xor (GetLocal (Name secondary-pointer)) (Const type-vect)) (Mul (Const 8) (Add (Const 1) (Sar e2 (Const int-shift))))))
                        (StoreHeap (i64) e3)
                        (Const val-void)
                    )
                    (err)))])))

(define (compile-cons e1 e2)
    (seq
        (get-tagged-heap-address type-cons) ; The return value.
        (GetGlobal (Name heap-name))        ; The first cell of the cons.
        (increment-heap-pointer)
        (GetGlobal (Name heap-name))        ; The second cell of the cons.
        (increment-heap-pointer)
        (StoreHeap (i64) e1)
        (StoreHeap (i64) e2)))

(define (compile-make-heap-vector e1 e2 type)
    (WatIf (Eqz e1) (Const type) (let ((loop-name (gensym 'loop))) (seq
        (get-tagged-heap-address type)
        (GetGlobal (Name heap-name))
        (increment-heap-pointer)
        (StoreHeap (i64) (Sar e1 (Const int-shift))) ; Write the length of the vector.
        (SetLocal (Name internal-pointer) (Sar e1 (Const int-shift)))
        (Loop (seq
            (GetGlobal (Name heap-name))
            (increment-heap-pointer)
            (StoreHeap (i64) e2)
            (SetLocal (Name internal-pointer) (Sub (GetLocal (Name internal-pointer)) (Const 1)))
            (BranchIf (Gt (GetLocal (Name internal-pointer)) (Const 0)) loop-name)
        ) loop-name)))))

;; Gets by index in an array-type structure (vector or string currently).
(define (compile-ref e1 e2 type)
    (seq
        (SetLocal (Name 'assert_scratch) (load-from-heap e1 type (Const 0)))
        (WatIf (64->32 (And 
                    (32->64 (Ge (Sar e2 (Const int-shift)) (Const 0))) 
                    (32->64 (Lt (Sar e2 (Const int-shift)) (GetLocal (Name 'assert_scratch))))))

            (load-from-heap e1 type (Mul (Const 8) (Add (Const 1) (Sar e2 (Const int-shift)))))
            (err)
        )
))

(define (compile-let id e1 e2 c)
    (seq (Comment "compiling let")
         (GetGlobal (Name stack-name))
         (StoreHeap (i64) (compile-e e1 c))
         (SetGlobal (Name stack-name) (AddT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) 8)))
         (compile-e e2 (cons (cons 'local id) c))
         (Comment "done compiling let")
    ))

;; Pushes the given value to the stack.
(define (push-to-stack e c) 
    (seq
        (GetGlobal (Name stack-name))
        (StoreHeap (i64) (compile-e e c))
        (SetGlobal (Name stack-name) (AddT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) 8))) ; Add pointer simulate pushing.
))

(define (push-to-stack-precompiled thing-to-push)
    (seq
        (GetGlobal (Name stack-name))
        (StoreHeap (i64) thing-to-push)
        (SetGlobal (Name stack-name) (AddT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) 8))))) ; Add pointer simulate pushing.

;; Pops the value at the top of the stack.
(define (pop-stack)
    (seq
        (WatIf (Eq (GetGlobal (Name stack-name)) (ConstT (i32) top-stack-address)) (err) 
        (SetGlobal (Name stack-name) (SubT (i32) (GetGlobal (Name stack-name)) (ConstT (i32) 8))) ; Decrement pointer simulate popping.
        (LoadHeap (i64) (GetGlobal (Name stack-name))) ; Retrieve the value here.
)))

;; Increments the heap pointer to the next available position.
(define (increment-heap-pointer)
    (seq (SetGlobal (Name heap-name) (AddT (i32) (GetGlobal (Name heap-name)) (ConstT (i32) 8)))))

;; Increments the heap pointer by a specified amount
(define (multi-increment-heap-pointer n)
    (seq (SetGlobal (Name heap-name) (AddT (i32) (GetGlobal (Name heap-name)) (ConstT (i32) n)))))

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


(define (compile-string s)
 (let ((len (string-length s)))
    (if (zero? len) (seq (Const type-str)) (seq
        (get-tagged-heap-address type-str)
        (GetGlobal (Name heap-name))
        (increment-heap-pointer)
        (StoreHeap (i64) (Const len))
        (compile-string-chars (string->list s))
    ))
))

(define (compile-string-chars cs)
    (match cs
        ['() (seq)]
        [(cons c cs) (seq
            (GetGlobal (Name heap-name))
            (increment-heap-pointer)
            (StoreHeap (i64) (Const (imm->bits c)))
            (compile-string-chars cs))]))

;; Helper function for getting a value from the heap and pushing it's value to the stack.
(define (load-from-heap e type offset)
    (seq
        (LoadHeap (i64) (64->32 (Add offset (Xor (Const type) e))))))

(define (load-from-heap-by-address type offset address)
    (seq
        (LoadHeap (i64) (64->32 (Add (Const offset) (Xor (Const type) address))))))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3 c)
    (WatIf (Ne (compile-e e1 c) (Const val-false))
        (compile-e e2 c)
        (compile-e e3 c)))

;; Id [Listof Id] Expr CEnv -> Asm
(define (compile-lam f xs e c)
  (let ((fvs (fv (Lam f xs e))))
    (seq (Comment "compiling lam")
         (get-tagged-heap-address type-proc)
         (Comment "putting function index on heap")
         (GetGlobal (Name heap-name))
         (StoreHeap (i64) (lookup-function-table f))   ;; get the function index and put it on heap
         (Comment "putting free vars on heap")
         (free-vars-to-heap fvs c 8)
         (Comment "incrementing heap pointer")
         (multi-increment-heap-pointer (* 8 (add1 (length fvs)))) ;; increment heap pointer
         (Comment "done compiling lam"))))

;; [Listof Id] CEnv Int -> Asm
;; Copy the values of given free variables into the heap at given offset
(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq (AddT (i32) (GetGlobal (Name heap-name)) (ConstT (i32) off))
          (StoreHeap (i64) (compile-variable x c))
          (free-vars-to-heap fvs c (+ off 8)))]))

;; [Listof Id] Int -> Asm
;; Copy the closure environment at given offset to stack
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (push-to-stack-precompiled (load-from-heap-by-address type-proc off (GetLocal (Name lam_heap_loc))))
          (copy-env-to-stack fvs (+ 8 off)))]))

;; (might need to pass around some context for this, or use a global variable)
(define (lookup-function-table f)
    (seq (Comment "function table index, looked up at compile time") 
         (Const (function-table-helper f function-names))))
(define (function-table-helper f ts)
    (match ts
        ['() (error (string-append (symbol->string f) ": function name does not exist in function table"))]
        [(cons t ts) (if (symbol=? t f) 0 (add1 (function-table-helper f ts)))]))

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
       [#t (match type
            ['local 0]
            ['param param])]
       [#f (match type
            ['local (+ 8 (lookup x rest))]
            ['param (lookup x rest)]
       )])])) 
       
(define (err)
    (Call 'error)
)

(define (assert-type mask type)
    (λ (arg)
        (seq (SetLocal (Name assert_scratch) arg)
             (WatIf (Ne (And (GetLocal (Name assert_scratch)) mask) type) 
                    (err) 
                    (GetLocal (Name assert_scratch))))))

(define assert-integer
    (assert-type (Const mask-int) (Const type-int)))
(define assert-char
    (assert-type (Const mask-char) (Const type-char)))
(define assert-box
    (assert-type (Const ptr-mask) (Const type-box)))
(define assert-cons
    (assert-type (Const ptr-mask) (Const type-cons)))
(define assert-string
    (assert-type (Const ptr-mask) (Const type-str)))
(define assert-vector
    (assert-type (Const ptr-mask) (Const type-vect)))
(define assert-proc
    (assert-type (Const ptr-mask) (Const type-proc)))

(define (assert-codepoint arg)
    (seq (SetLocal (Name assert_scratch) arg)
         (WatIf (Lt (GetLocal (Name assert_scratch)) (imm->bits 0)) 
                (err)
                (WatIf  (Gt (GetLocal (Name assert_scratch)) (imm->bits 1114111)) 
                        (err)
                        (WatIf (And (Ge (GetLocal (Name assert_scratch)) (imm->bits 55295)) 
                                    (Le (GetLocal (Name assert_scratch)) (imm->bits 57344))) 
                               (err) 
                               (GetLocal (Name assert_scratch)))))))

(define (assert-byte arg)
    (seq (SetLocal (Name assert_scratch) arg)
         (WatIf (Lt (GetLocal (Name assert_scratch)) (imm->bits 0)) 
                (err)
                (WatIf (Gt (GetLocal (Name assert_scratch)) (imm->bits 255)) 
                       (err) 
                       (GetLocal (Name assert_scratch))))))
