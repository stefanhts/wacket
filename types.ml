type op = Add | Sub | Mult | Div | Concat | Greater | Less | GreaterEqual | LessEqual | Equal | NotEqual | Or | And

type var = string

type value = 
  | Int of int
  | Bool of bool
  | String of string

and expr = 
  | Value of value
  | ID of var
  | Fun of var * expr
  | Not of expr
  | Binop of op * expr * expr
  | Let of var * bool * expr * expr
