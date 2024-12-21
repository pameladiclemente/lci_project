
(* The type of tokens. *)

type token = 
  | TIMES
  | THEN
  | RPAREN
  | PLUS
  | OR
  | NOT
  | MODULO
  | MINUS
  | LT
  | LPAREN
  | LETFUN
  | LET
  | LE
  | INT of (int)
  | IN
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FUN
  | EQ
  | EOF
  | ELSE
  | DIVIDE
  | BOOL of (bool)
  | ASSIGN
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (term)
