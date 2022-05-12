#lang racket

(provide (all-defined-out))

;; type Module = (Module (Listof definitions))
(struct Module (ds) #:prefab)
;; type Import = (Import (modulename funcname FuncSignature)) 
(struct Import (m f fs) #:prefab)
;; type MemoryExport = ()
(struct MemoryExport () #:prefab)
;; type Export = (Export (name ExportFuncSignature))
(struct Export (n d) #:prefab)
;; type Global = (Global (name type Const))
(struct Global (n t i) #:prefab)
;; type Func = (Func (FuncSignature (Listof Locals) Body))
(struct Func (s ls b) #:prefab)
;; type FuncList = (FuncList (Listof Func))
(struct FuncList (fs) #:prefab)
;; type FuncSignature = (FuncSignature (name? (Listof Params) Result?))
;; replace r with empty list for no return value
(struct FuncSignature (n ps r) #:prefab)
;; type ExportFuncSignature = (ExportFuncSignature (name))
(struct ExportFuncSignature (n) #:prefab)
;; type Result = (Result (Type))
(struct Result (t) #:prefab)
;; type Local = (Local (name? Type))
(struct Local (n t) #:prefab)
;; type Type = i32 | i64 | f32 | f64
(struct i32 () #:prefab)
(struct i64 () #:prefab)
(struct f32 () #:prefab)
(struct f64 () #:prefab)
;; type Body = (Body (Listof Instructions))
(struct Body (is) #:prefab)

;; WASM Instructions

;; type WatIf = (WatIf (Instruction Instruction Instruction))
(struct WatIf (p t f) #:prefab)

(struct Eq (i1 i2) #:prefab)

(struct Ne (i1 i2) #:prefab)

(struct Eqz (i) #:prefab)

(struct Lt (i1 i2) #:prefab)

(struct Gt (i1 i2) #:prefab)

(struct Le (i1 i2) #:prefab)

(struct Ge (i1 i2) #:prefab)
;; Add which takes an arbitrary type.
(struct AddT (t i1 i2) #:prefab)
;; Convenience for adding 64 bit integers.
(struct Add (i1 i2) #:prefab)
;; Sub which takes an arbitrary type.
(struct SubT (t i1 i2) #:prefab)
;; Convenience for adding 64 bit integers.
(struct Sub (i1 i2) #:prefab)

(struct Mul (i1 i2) #:prefab)

(struct Div (i1 i2) #:prefab)

(struct And (i1 i2) #:prefab)

(struct Or (i1 i2) #:prefab)

(struct Xor (i1 i2) #:prefab)

(struct Sar (i n) #:prefab)

(struct Sal (i n) #:prefab)

(struct 32->64 (i) #:prefab)

(struct 64->32 (i) #:prefab)

(struct GetLocal (n) #:prefab)

(struct SetLocal (n i) #:prefab)

(struct GetGlobal (n) #:prefab)

(struct SetGlobal (n v) #:prefab)
;; type LoadHeap = (Type Index)
(struct LoadHeap (t i) #:prefab)
;; This type implicitly requires the address to be stored on the stack before.
;; type StoreHeap = (Type Value)
(struct StoreHeap (t v) #:prefab)
;; type Loop = (Instructions LoopName)
(struct Loop (e n) #:prefab)
;; type BranchIf = (Predicate LoopName)
(struct BranchIf (p n) #:prefab)

;; type Name = (Name (n))
(struct Name (n) #:prefab)

;; Const of an arbitrary type.
(struct ConstT (t n) #:prefab)
;; type Const = (Const (n))
(struct Const (n) #:prefab)
;; type Start = (Start (funcidx))
(struct Start (f) #:prefab)
;; type Call = (Call (funcName))
(struct Call (f) #:prefab)
;; type Drop = (Drop)
;; this is for popping things off stack and discarding them
(struct Drop () #:prefab)
;; type Load = (Load)
;; Used when the value is known to be stored on the stack.
(struct Load () #:prefab)
;; type Table = (Table (length))
(struct Table (n) #:prefab)
;; type Type = (TypeDec ())
(struct TypeDec () #:prefab)
;; type Type = (Type ())
(struct Type () #:prefab)
;; type Elem = (Elem (listof functions))
(struct Elem (fs))


;; (U Instruction Asm) ... -> Asm
;; Convenient for sequencing instructions or groups of instructions
(define (seq . xs)
  (foldr (λ (x is)
           (if (list? x)
               (append x is)
               (cons x is)))
         '()
         xs))