#lang racket
(require "ast.rkt")

(provide wat-out)

;; wat ast -> string
(define (wat-out w)
    (parse-module w))

(define (tabs n)
    (make-string (* 8 n) #\space))

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
    (match (list m f)
        ['() (error "parse error: import")]
        [(list '() '() '()) (error "parse error: missing names")]
        [(list m f fs) (string-append

        )]

    )
)
(define (parse-export n d ntabs) "TODO: parse-export")
(define (parse-func s ls b ntabs) "TODO: parse-func")
(define (parse-funcsig n ps r) "TODO: implement this")

(define (parse-start f ntabs)
    (string-append 
        (tabs ntabs) "(start $" (symbol->string f) ")" "\n"))
