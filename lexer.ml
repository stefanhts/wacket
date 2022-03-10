open TokenTypes
open Str

(* Regex to match each token *)
(* match spaces *)
let re_space = Str.regexp "[ \t\n]+"
(* match right parenthesis *)
let re_rightpar = Str.regexp ")[ \t\n]*"
(* match left parenthesis *)
let re_leftpar = Str.regexp "([ \t\n]*"
(*  match equals *)
let re_equals = Str.regexp "=[ \t\n]*"

(* match neq*)
