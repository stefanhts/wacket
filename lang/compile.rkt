#lang racket
(require "ast.rkt" "../wat/ast.rkt" "types.rkt")
(provide compile)
(define (compile e)
        (Module (list (Export 'main (ExportFuncSignature 'main)) 
                      (Func (FuncSignature 'main '() (Result (i64))) '() 
                        (Body (seq (compile-e e)))))))

(define (compile-e e c)
    (match e
        [(Var v) (compile-var v c)]
        [(Int n) (Const (imm->bits n))]
        [(Bool b) (Const (imm->bits b))]
        [(Char c) (Const (imm->bits c))]
        [(Prim1 p e) (compile-prim1 p e c)]
        [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
        [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
        [(Let x e1 e2 c) (compile-let x e1 e2 c)]
        ))

(define (compile-var v c)
   ;; TODO: how do i put the variable on the stack... 
)

(define (lookup x cenv)
    (match cenv
        ['() (error "undefined variable:" x)]
        [(cons y rest)
            (match (eq? x y
                [#t 0] 
                [#f (+ 8 (lookup x rest))]
            ))]
    )
    )

(define (compile-prim1 p e c)
    (match p
        ['add1 (Add (compile-e e c) (Const (imm->bits 1)))]
        ['sub1 (Sub (compile-e e c) (Const (imm->bits 1)))] 
        ['zero?
            (WatIf (Eqz (compile-e c))
                (Const val-true)
                (Const val-false))]
        ['char?
            (WatIf (Eqz (Xor (And (compile-e p c) (Const mask-char)) (Const type-char)))
                (Const val-true) 
                (Const val-false))]
        ['char->integer
            (Sal (Sar (compile-e e c) (Const char-shift)) (Const int-shift))]
        ['integer->char
            (Xor (Sal (Sar (compile-e e c) (Const int-shift)) (Const char-shift)) (Const type-char))]))

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
            [_ (error "compile error: unexpec")]
        )
   ) 
)

(define (compile-let x e1 e2 c)
    (seq (SetLocal (Name x) (compile-e e1 c))
         (compile-e e2 c))
)

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3 c)
    (WatIf (Ne (compile-e e1 c) (Const val-false c))
        (compile-e e2 c)
        (compile-e e3 c)
    )
)
