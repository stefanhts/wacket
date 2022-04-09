#lang racket
(require "ast.rkt")

(provide wat-out)

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
        [_ (error "expected (Module ...) at  top level")]))

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
(define (parse-export n fs ntabs) 
    (match (cons n fs)
        ['() (error "parse error: empty export")]
        [(cons n fs) 
            (string-append
                (tabs ntabs) "(export \"" (symbol->string n) "\" " (parse-funcsig fs ntabs) ")\n"
            )]
    
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
        [(cons (ZrInst n) is) (string-append
            (tabs ntabs) "<ZrInst>" "\n"
            (parse-instruction-list is ntabs))]
        [(cons (UnInst n i) is) (string-append
            (tabs ntabs) "<UnInst>" "\n"
            (parse-instruction-list is ntabs))]
        [(cons (BiInst n i1 i2) is) (string-append
            (tabs ntabs) "<BiInst>" "\n"
            (parse-instruction-list is ntabs))]
        [x (begin (display x) (error "WAT parse error: instruction not recognized:"))]))

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
        [(FuncSignature n ps r) (string-append ("$" (symbol->string n) (parse-params ps ntabs) ))]
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

