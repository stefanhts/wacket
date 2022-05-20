#lang racket
  (provide parse parse-define parse-e)
  (require "ast.rkt")

  (define defined-function-names '())

  (define (get-defined-function-names s)
    (match s
      [(cons _ '()) '()]
      [(cons (list 'define (list-rest (? symbol? f) _) _) s) (cons f (get-defined-function-names s))]))

  (define (parse s)
    (begin (set! defined-function-names (get-defined-function-names s))
      (parse-after-defined-functions-found s)))

  ;; [Listof S-Expr] -> Prog
  (define (parse-after-defined-functions-found s)
    (match s
      [(cons (and (cons 'define _) d) s)
       (match (parse-after-defined-functions-found s)
         [(Prog ds e)
          (Prog (cons (parse-define d) ds) e)])]
      [(cons e '()) (Prog '() (parse-e e))]
      [_ (error "program parse error")]))
   
  ;; S-Expr -> Defn
  (define (parse-define s)
    (match s
      [(list 'define (list-rest (? symbol? f) xs) e)
       (if (andmap symbol? xs)
           (Defn f xs (parse-e e))
           (error "parse definition error"))]
      [_ (error "Parse defn error" s)]))
   
  ;; S-Expr -> Expr
  (define (parse-e s)
    (match s
      [(? integer?)                  (Int s)]
      [(? boolean?)                  (Bool s)]
      [(? char?)                     (Char s)]
      [(? string?)                   (Str s)]
      ['eof                          (Eof)]
      [(? symbol?)                   (Var s)]
      [(list 'quote (list))          (Empty)]
      [(list (? (op? op0) p0))       (Prim0 p0)]
      [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
      [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
      [(list (? (op? op3) p3) e1 e2 e3)
       (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
      [(list 'begin e1 e2)
       (Begin (parse-e e1) (parse-e e2))]
      [(list 'if e1 e2 e3)
       (If (parse-e e1) (parse-e e2) (parse-e e3))]
      [(list 'let (list (list (? symbol? x) e1)) e2)
       (Let x (parse-e e1) (parse-e e2))]
      [(list (or 'lambda 'λ) xs e)
       (if (and (list? xs)
                (andmap symbol? xs))
           (Lam (gensym 'lambda) (append xs (list lam_heap_loc)) (parse-e e))
           (error "parse lambda error"))]
      [(cons (? (defined-function?) f) es)
        (AppDef f (map parse-e es))]
      [(cons e es)
       (App (parse-e e) (map parse-e es))]
      [_ (error "Parse error" s)]))
   
  (define op0
    '(read-byte peek-byte void))
   
  (define op1
    '(add1 sub1 zero? char? write-byte eof-object?
           integer->char char->integer
           box unbox empty? cons? box? car cdr
           vector? vector-length string? string-length))
  (define op2
    '(+ - < = * / << >> and or xor cons eq? make-vector vector-ref make-string string-ref))
  (define op3
    '(vector-set!))
   
  (define (op? ops)
    (λ (x)
      (and (symbol? x)
           (memq x ops))))

  (define (defined-function?)
    (lambda (x)
      (and (symbol? x)
           (memq x defined-function-names))))
