#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "../wat/printer.rkt")

(define (main fn)
    (let ((p (open-input-file fn)))
    (begin
    (read-line p)
    (wat-display (compile (parse (read p))))
    (close-input-port p)))
)