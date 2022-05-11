#lang racket
(require "ast.rkt" "../wat/ast.rkt" "types.rkt")
(provide compile)
(define (compile e)
        (Module (list (Import 'io 'read (FuncSignature 'readByte '() (Result (i64))))
                      (Import 'io 'write (FuncSignature 'writeByte (list '_) (Result (i64))))
                      (Import 'io 'peek (FuncSignature 'peekByte '() (Result (i64))))
                      (Export 'main (ExportFuncSignature 'main)) 
                      (Func (FuncSignature 'main '() (Result (i64))) '() 
                        (Body (seq (compile-e e)))))))

(define (compile-e e)
    (match e
        [(Int n) (Const (imm->bits n))]
        [(Bool b) (Const (imm->bits b))]
        [(Char c) (Const (imm->bits c))]
        [(Eof) (Const (imm->bits eof))]
        [(Prim0 p) (compile-prim0 p)]
        [(Prim1 p e) (compile-prim1 p e)]
        [(If e1 e2 e3) (compile-if e1 e2 e3)]
        [(Begin e1 e2) (compile-begin e1 e2)]))

(define (compile-prim0 p)
    (match p
        ['void (Const val-void)]
        ['read-byte (Call 'readByte)]
        ['peek-byte (Call 'peekByte)]))

(define (compile-prim1 p e)
    (match p
        ['add1 (Add (compile-e e) (Const (imm->bits 1)))]
        ['sub1 (Sub (compile-e e) (Const (imm->bits 1)))] 
        ['zero?
            (WatIf (Eqz (compile-e))
                (Const val-true)
                (Const val-false))]
        ['char?
            (WatIf (Eqz (Xor (And (compile-e p) (Const mask-char)) (Const type-char)))
                (Const val-true) 
                (Const val-false))]
        ['char->integer
            (Sal (Sar (compile-e e) (Const char-shift)) (Const int-shift))]
        ['integer->char
            (Xor (Sal (Sar (compile-e e) (Const int-shift)) (Const char-shift)) (Const type-char))]
        ['eof-object? 
            (WatIf (Eq (compile-e e) (Const val-eof))
                (Const val-true)
                (Const val-false))]
        ['write-byte (seq (compile-e e) (Call 'writeByte))]))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
    (WatIf (Ne (compile-e e1) (Const val-false))
        (compile-e e2)
        (compile-e e3)
    )
)

(define (compile-begin e1 e2)
    (seq (compile-e e1)
         (Drop)
         (compile-e e2)))
