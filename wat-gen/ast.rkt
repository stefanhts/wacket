#lang racket

(provide Module Imports Exports Funcs Start)
;; type Module = (Module (definitions))
(struct Module (ds))
;; type Imports = (Imports (imports)) 
(struct Imports (is))
;; type Exports = (Exports (exports))
(struct Exports (es))
;; type Type = (Type ())
;; TODO: Type struct
;; type Funcs = (Funcs (functions))
(struct Funcs (fs))
;; type Start = (Start (function))
(struct Start (f))