#lang racket

(provide (all-defined-out))

(define lam_heap_loc (gensym 'lam_heap_loc))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)

;; type Expr =
;; | (Eof)
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Prim0 Op0)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (If Expr Expr Expr)
;; | (Begin Expr Expr)
;; | (Let Id Expr Expr)
;; | (Var Id)
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1 | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
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
(struct Begin (e1 e2)      #:prefab)
(struct Let   (x e1 e2)    #:prefab)
(struct Var   (x)          #:prefab)
(struct App   (e es)       #:prefab)
(struct AppDef (f es)      #:prefab)
(struct Lam   (f xs e)     #:prefab)
