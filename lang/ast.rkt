#lang racket
(provide (all-defined-out))

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (If Expr Expr Expr)
;; type Op1 = 'add1 | 'sub1 | 'zero?
;;         | 'char? | 'integer->char | 'char->integer
(struct Int (i)         #:prefab)
(struct Bool (b)        #:prefab)
(struct Char (c)        #:prefab)
(struct Prim1 (p e)     #:prefab)
(struct Prim2 (p e1 e2) #:prefab)
(struct If (e1 e2 e3)   #:prefab)