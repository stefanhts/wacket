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
        [x (error "expected (Module ...) at  top level" x)]))

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
            (parse-definitions ds ntabs))]))

(define (parse-import m f fs ntabs) 
    (match (list m f fs)
        ['() (error "parse error: import")]
        [(list '() '() '()) (error "parse error: missing names")]
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
        [_ (error "WAT parse error: should be FuncSignature (input)")]))

(define (parse-export n fs ntabs) 
    (match (cons n fs)
        ['() (error "parse error: empty export")]
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
        [_ (error "WAT parse error: should be ExportFuncSignature")]))

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
        [_ (error "WAT parse error: expected local")]))
(define (parse-body b ntabs)
    (match b
        [(Body is) (parse-instruction-list is ntabs)]
        [x (begin (display x) (error "WAT parse error: expected function body"))]))
(define (parse-instruction-list is ntabs)
    (match is
        ['() ""]
        [(cons (Result t) is) (string-append (tabs ntabs) "(result " (wattype->string t) ")\n" (parse-instruction-list is ntabs))]
        [(cons (Name n) is) (string-append (tabs ntabs) "$" (symbol->string n) "\n" (parse-instruction-list is ntabs))]
        [(cons (Inst n '()) is) (string-append          ;; no arguments
            (tabs ntabs) "(" (symbol->string n) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons (Inst n sub-is) is) (string-append       ;; has arguments
            (tabs ntabs) "(" (symbol->string n) "\n"
            (parse-instruction-list sub-is (add1 ntabs))
            (tabs ntabs) ")\n"
            (parse-instruction-list is ntabs))]
        [(cons (Const n) is) (string-append
            (tabs ntabs) "(i64.const " (number->string n) ")\n"
            (parse-instruction-list is ntabs))]
        [x (error "WAT parse error: instruction not recognized:" x)]))

(define (wattype->string t)
    (match t 
        [(i32) "i32"]
        [(i64) "i64"]
        [(f32) "f32"]
        [(f64) "f64"]))

(define (parse-funcsig s ntabs)
    (match s
        [(FuncSignature n ps (Result t)) (string-append
           "$" (symbol->string n) (parse-params ps ntabs) " (result " (wattype->string t) ")" 
        )]
        [_ (error "WAT parse error: should be FuncSignature")]))

(define (parse-params ps ntabs)
    (match ps
        ['() ""]
        [(cons (Param p t) ps) (string-append " (param " (wattype->string t) ")")] 
    )
)

(define (parse-start f ntabs)
    (string-append 
        (tabs ntabs) "(start $" (symbol->string f) ")" "\n"))

