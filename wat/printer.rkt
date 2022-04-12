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
        [(cons (Export n d) ds) (string-append
            (parse-export n d ntabs)
            (parse-definitions ds ntabs))]
        [(cons (Func s ls b) ds) (string-append
            (parse-func s ls b ntabs)
            (parse-definitions ds ntabs))]
        [(cons (Start f) ds) (string-append
            (parse-start f ntabs)
            (parse-definitions ds ntabs))]
        [(cons x _) (parse-error "Expected definition, got:" x)]
        [x (parse-error "Expected list of definitions, got:" x)]))

(define (parse-import m f fs ntabs) 
    (match (list m f fs)
        ['() (parse-error "Empty import")]
        [(list '() '() '()) (parse-error "Import missing names")]
        [(list m f fs) (string-append
            (tabs ntabs) "(import \"" (symbol->string m) "\" \"" (symbol->string f) "\" " (parse-funcsig fs ntabs) ")\n" 
        )]

    )
)
(define (parse-import-funcsig s ntabs)
    (match s
        [(FuncSignature n ps (Result t)) (string-append
           "(func $" (symbol->string n) (parse-params ps ntabs) " (result " (wattype->string t) "))" 
        )]
        [x (parse-error "Should be FuncSignature (input), was:" x)]))

(define (parse-export n fs ntabs) 
    (match (cons n fs)
        ['() (parse-error "Empty export")]
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

(define (parse-func s ls b ntabs) 
    (string-append
        (tabs ntabs) "(func " (parse-funcsig s ntabs) "\n"
        (parse-locals ls (add1 ntabs))
        (parse-body b (add1 ntabs))
        (tabs ntabs) ")\n"))
(define (parse-locals ls ntabs)
    (match ls
        ['() ""]
        [(cons (Local n t) ls) (string-append
            (tabs ntabs) "(local $" (symbol->string n) " " (wattype->string t) ")\n"
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
        [(cons (Result t) is) (string-append (tabs ntabs) "(result " (wattype->string t) ")\n" (parse-instruction-list is ntabs))]
        [(cons (Name n) is) (string-append (tabs ntabs) "$" (symbol->string n) "\n" (parse-instruction-list is ntabs))]
        [(cons (Inst n '()) is) (string-append          ;; no arguments
            (tabs ntabs) "(" (instruction->string n) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons (Inst n sub-is) is) (string-append       ;; has arguments
            (tabs ntabs) "(" (instruction->string n) "\n"
            (parse-instruction-list sub-is (add1 ntabs))
            (tabs ntabs) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons (Const n) is) (string-append
            (tabs ntabs) "(i64.const " (number->string n) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons x _) (parse-error "Instruction not recognized:" x)]
        [x (parse-error "Expected instruction list, got:" x)]))

(define (wattype->string t)
    (match t 
        [(i32) "i32"]
        [(i64) "i64"]
        [(f32) "f32"]
        [(f64) "f64"]
        [x (parse-error "Unrecognized type; expected one of (i32), (i64), (f32), (f64), got:" x)]))

(define (instruction->string i)
    (let ((supported-instructions '(
        i64.eq
        i64.ne
        i64.eqz
        i64.lt_s
        i64.gt_s
        i64.le_s
        i64.ge_s
        i64.add
        i64.sub
        i64.mul
        i64.div_s
        i64.and
        i64.or
        i64.xor
        i64.extend_i32_s
        local.set
        local.get
        if
    ))) (match i
        [(? (lambda (x) (memq x supported-instructions)) i) (symbol->string i)]
        [x (parse-error "Unsupported instruction:" x)])))

(define (parse-funcsig s ntabs)
    (match s
        [(FuncSignature n ps (Result t)) (string-append
           "$" (symbol->string n) (parse-params ps ntabs) " (result " (wattype->string t) ")" 
        )]
        [x (parse-error "Should be FuncSignature, but is:" x)]))

(define (parse-params ps ntabs)
    (match ps
        ['() ""]
        [(cons (Param p t) ps) (string-append " (param " (wattype->string t) ")")] 
    )
)

(define (parse-start f ntabs)
    (string-append 
        (tabs ntabs) "(start $" (symbol->string f) ")" "\n"))

(define (parse-error s x) 
    (error (string-append "WAT parse error. " s) x))
