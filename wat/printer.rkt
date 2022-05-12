#lang racket
(require "ast.rkt")

(provide wat-display wat-out)

;; wat ast -> void
(define (wat-display w) (begin (write-string (wat-out w)) (void)))

;; wat ast -> string
(define (wat-out w)
    (parse-module w))

(define (tabs n)
    (make-string (* 4 n) #\space))

(define (parse-module w)
    (match w
        [(Module ds) (string-append 
            "(module" "\n" 
            (parse-definitions ds 1)
            ")" "\n")]
        [x (parse-error "Expected (Module ...) at  top level, found:" x)]))

(define (parse-definitions ds ntabs)
    (match ds
        ['() ""]
        [(cons (Import m f fs) ds) (string-append 
            (parse-import m f fs ntabs)
            (parse-definitions ds ntabs))]
        [(cons (MemoryExport) ds) (string-append
            (tabs ntabs) "(memory (export \"memory\") 2)\n"
            (parse-definitions ds ntabs)
        )]
        [(cons (Export n d) ds) (string-append
            (parse-export n d ntabs)
            (parse-definitions ds ntabs))]
        [(cons (Global n t i) ds) (string-append
            (parse-global n t i ntabs)
            (parse-definitions ds ntabs))]
        [(cons (Func s ls b) ds) (string-append
            (parse-func s ls b ntabs)
            (parse-definitions ds ntabs))]
        [(cons (FuncList fs) ds) (string-append
            (parse-funclist fs ntabs)
            (parse-definitions ds ntabs))]
        [(cons (Start f) ds) (string-append
            (parse-start f ntabs)
            (parse-definitions ds ntabs))]
        [(cons x _) (parse-error "Expected definition, got:" x)]
        [x (parse-error "Expected list of definitions, got:" x)]))

(define (parse-import m f fs ntabs) 
    (match (list m f fs)
        ['() (parse-error-noarg "Empty import")]
        [(list '() '() '()) (parse-error-noarg "Import missing names")]
        [(list m f fs) (string-append
            (tabs ntabs) "(import \"" (symbol->string m) "\" \"" (symbol->string f) "\" (func " (parse-funcsig fs ntabs) "))\n" 
        )]

    )
)
(define (parse-import-funcsig s ntabs)
    (match s
        [(FuncSignature n ps (Result t)) (string-append
           "(func $" (symbol->string n) (parse-params ps ntabs) " (result " (wattype->string t) "))" 
        )]
        ; [(FuncSignature n ps '()) (string-append
        ;    "(func $" (symbol->string n) (parse-params ps ntabs) ")" 
        ; )]
        [x (parse-error "Should be FuncSignature (input), was:" x)]))

(define (parse-export n fs ntabs) 
    (match (cons n fs)
        ['() (parse-error-noarg "Empty export")]
        [(cons n fs) 
            (string-append
                (tabs ntabs) "(export \"" (symbol->string n) "\" " (parse-export-funcsig fs ntabs) ")\n"
            )]
    
    ))
(define (parse-export-funcsig s ntabs)
    (match s
        [(ExportFuncSignature n) (string-append
           "(func $" (symbol->string n) ")" 
        )]
        [x (parse-error "Expected ExportFuncSignature, got:" x)]))

(define (parse-global n t i ntabs)
    (match i
        [(Const i) (string-append 
        (tabs ntabs)
        "(global $" (symbol->string n) " (mut " (wattype->string t) ") (" (wattype->string t) ".const " 
        (number->string i) "))\n")]
        [x (parse-error "Expected const, got " x)]
))

(define (parse-func s ls b ntabs) 
    (string-append
        (tabs ntabs) "(func " (parse-funcsig s ntabs) "\n"
        (parse-locals ls (add1 ntabs))
        (parse-body b (add1 ntabs))
        (tabs ntabs) ")\n"))
(define (parse-locals ls ntabs)
    (match ls
        ['() ""]
        [(cons l ls) (string-append
            (tabs ntabs) "(local $" (symbol->string l) " i64)\n"
            (parse-locals ls ntabs))]
        [(cons x _) (parse-error "Expected local in locals section, got" x)]
        [x (parse-error "Expected list of locals, got" x)]))
(define (parse-body b ntabs)
    (match b
        [(Body is) (parse-instruction-list is ntabs)]
        [x (parse-error "Expected function body, got:" x)]))
(define (parse-instruction-list is ntabs)
    (match is
        ['() ""]
        [(cons (Result t) is) (string-append 
            (tabs ntabs) "(result " (wattype->string t) ")\n" 
            (parse-instruction-list is ntabs))]
        [(cons (Name n) is) (string-append 
            (tabs ntabs) "$" (symbol->string n) "\n" 
            (parse-instruction-list is ntabs))]
        [(cons (Call f) is) (string-append (tabs ntabs) "(call $" (symbol->string f) ")\n" 
        (parse-instruction-list is ntabs))]
        ; ;; the following is used for assert-type
        ; [(cons (WatIf p t '()) is) (string-append
        ;     (tabs ntabs) "(if\n"
        ;     (parse-instruction-list (seq p t) (add1 ntabs))
        ;     (tabs (add1 ntabs)) "(nop)"
        ;     (tabs ntabs) ")\n"
        ;     (parse-instruction-list is ntabs))]
        [(cons (WatIf p t f) is) (string-append
            (tabs ntabs) "(if (result i64)\n"
            (parse-instruction-list (seq p t f) (add1 ntabs))
            (tabs ntabs) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons (ConstT t n) is) (string-append
            (tabs ntabs) "(" (wattype->string t) ".const " (number->string n) ")\n"
            (parse-instruction-list is ntabs)
        )]
        [(cons (Const n) is) (string-append
            (tabs ntabs) "(i64.const " (number->string n) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons (app instr-lit (list lit sub-is)) next-is) (string-append
            (tabs ntabs) "(" lit "\n"
            (parse-instruction-list sub-is (add1 ntabs))
            (tabs ntabs) ")\n"
            (parse-instruction-list next-is ntabs))]
        [(cons (GetLocal (Name n)) is) (string-append (tabs ntabs) "(local.get $" (symbol->string n) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons (SetLocal (Name n) i) is) (string-append (tabs ntabs) "(local.set $" (symbol->string n) "\n"
            (parse-instruction-list (seq i) (add1 ntabs))
            (tabs ntabs) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons (GetGlobal (Name n)) is) (string-append (tabs ntabs) "(global.get $" (symbol->string n) ")\n"
            (parse-instruction-list is ntabs)
        )]
        [(cons (SetGlobal (Name n) i) is) (string-append (tabs ntabs) "(global.set $" (symbol->string n) "\n"
            (parse-instruction-list (seq i) (add1 ntabs))
            (tabs ntabs) ")\n"
            (parse-instruction-list is ntabs)
        )]
        [(cons (LoadHeap t i) is) (string-append (tabs ntabs) "(" (wattype->string t) ".load\n" 
            (parse-instruction-list (seq i) (add1 ntabs))
            (tabs ntabs) ")\n"
            (parse-instruction-list is ntabs)
        )]
        [(cons (StoreHeap t v) is) (string-append (tabs ntabs) "(" (wattype->string t) ".store\n" 
            (parse-instruction-list (seq v) (add1 ntabs)) ; The value to store.
            (tabs ntabs) ")\n"
            (parse-instruction-list is ntabs)
        )]
        [(cons (Drop) is) (string-append (tabs ntabs) "(drop)\n"
            (parse-instruction-list is ntabs)
        )]
        [(cons x _) (parse-error "Instruction not recognized:" x)]
        [x (parse-error "Expected instruction list, got:" x)]))

;; Instruction struct -> (list string (list of Instructions))
(define (instr-lit i)
    (match i
        [(Eq  i1 i2) (list "i64.eq"    (seq i1 i2))]
        [(Ne  i1 i2) (list "i64.ne"    (seq i1 i2))]
        [(Lt  i1 i2) (list "i64.lt_s"  (seq i1 i2))]
        [(Gt  i1 i2) (list "i64.gt_s"  (seq i1 i2))]
        [(Le  i1 i2) (list "i64.le_s"  (seq i1 i2))]
        [(Ge  i1 i2) (list "i64.ge_s"  (seq i1 i2))]
        [(AddT t i1 i2) (list 
            (string-append (wattype->string t) ".add") 
            (seq i1 i2))]
        [(Add i1 i2) (list "i64.add"   (seq i1 i2))]
        [(SubT t i1 i2)  (list 
            (string-append (wattype->string t) ".sub") 
            (seq i1 i2))]
        [(Sub i1 i2) (list "i64.sub"   (seq i1 i2))]
        [(Mul i1 i2) (list "i64.mul"   (seq i1 i2))]
        [(Div i1 i2) (list "i64.div_s" (seq i1 i2))]
        [(And i1 i2) (list "i64.and"   (seq i1 i2))]
        [(Or  i1 i2) (list "i64.or"    (seq i1 i2))]
        [(Xor i1 i2) (list "i64.xor"   (seq i1 i2))]
        [(Sar i1 i2) (list "i64.shr_s" (seq i1 i2))]
        [(Sal i1 i2) (list "i64.shl"   (seq i1 i2))]
        [(Eqz    i1) (list "i64.eqz"          (seq i1))]
        [(32->64 i1) (list "i64.extend_i32_s" (seq i1))]
        [(64->32 i1) (list "i32.wrap_i64"     (seq i1))]
        [_ #f]))

(define (wattype->string t)
    (match t 
        [(i32) "i32"]
        [(i64) "i64"]
        [(f32) "f32"]
        [(f64) "f64"]
        [x (parse-error "Unrecognized type; expected one of (i32), (i64), (f32), (f64), got:" x)]))

(define (parse-funcsig s ntabs)
    (match s
        [(FuncSignature n ps (Result t)) (string-append
           "$" (symbol->string n) (parse-params ps ntabs) " (result " (wattype->string t) ")" 
        )]
        [x (parse-error "Should be FuncSignature, but is:" x)]))

(define (parse-funclist fs ntabs)
    (match fs
        ['() ""]
        [(cons (Func f ls b) fs) (string-append 
                (parse-func f ls b ntabs) (parse-funclist fs ntabs))]
                )
)

(define (parse-params ps ntabs)
    (match ps
        ['() ""]
        [(cons p ps) (string-append " (param $" (symbol->string p) " i64) " (parse-params ps ntabs))] 
    )
)

(define (parse-start f ntabs)
    (string-append 
        (tabs ntabs) "(start $" (symbol->string f) ")" "\n"))

(define (parse-error s x) 
    (error (string-append "WAT parse error. " s) x))
(define (parse-error-noarg s)
    (error (string-append "WAT parse error. " s)))