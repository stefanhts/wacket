#lang racket
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define basic-lexer
    (lexer-srcloc
     ["\n" (token 'NEWLINE lexeme)]
     [whitespace (token lexeme #:skip? #t)]
     [(from/stop-before "rem" "\n") (token 'REM lexeme)]
     [(:or "print" "goto" "end" "+" ":" ";") (token lexeme lexeme)]
     [digits (token 'INTEGER (string->number lexeme))]
     [(:or (:seq (:? digits) "." digits)
           (:seq digits "."))
           (token 'DECIMAL (string->number lexeme))]
     [(:or (from/to "\"" "\"") (from/to "'" "'"))
      (token 'STRING
             (substring lexeme
                        1 (sub1 (string-length lexeme))))]
     ))

(provide basic-lexer)