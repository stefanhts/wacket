#lang racket
(require "ast.rkt")
(provide fv)

;; Expr -> [Listof Id]
;; List all of the free variables in e
(define (fv e)
  (remove-duplicates (fv* e)))

(define (fv* e)  
  (match e
    [(Var x)            (list x)]
    [(Prim1 p e)        (fv* e)]
    [(Prim2 p e1 e2)    (append (fv* e1) (fv* e2))]
    [(Prim3 p e1 e2 e3) (append (fv* e1) (fv* e2) (fv* e3))]
    [(If e1 e2 e3)      (append (fv* e1) (fv* e2) (fv* e3))]
    [(Begin e1 e2)      (append (fv* e1) (fv* e2))]
    [(Let x e1 e2)      (append (fv* e1) (remq* (list x) (fv* e2)))]
    [(App e1 es)        (append (fv* e1) (append-map fv* es))]
    [(Lam f xs e)       (remq* xs (fv* e))]
    [_                  '()]))
