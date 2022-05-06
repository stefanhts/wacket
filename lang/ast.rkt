#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (If Expr Expr Expr)
;; | (Let Id Expr Expr)
;; | (Var Id)
;; type Op1 = 'add1 | 'sub1 | 'zero?
;;         | 'char? | 'integer->char | 'char->integer
(struct Eof   ()           #:prefab)
(struct Empty ()           #:prefab)
(struct Str   (s)          #:prefab)
(struct Int   (i)          #:prefab)
(struct Bool  (b)          #:prefab)
(struct Char  (c)          #:prefab)
(struct Prim0 (p)          #:prefab)
(struct Prim1 (p e)        #:prefab)
(struct Prim2 (p e1 e2)    #:prefab)
(struct Prim3 (p e1 e2 e3) #:prefab)
(struct If    (e1 e2 e3)   #:prefab)
(struct Let   (x e1 e2)    #:prefab)
(struct Var   (x)          #:prefab)
(struct App   (f es)       #:prefab)