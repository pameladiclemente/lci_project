
(* The type of tokens. *)

type token = 
  | WITH
  | WHILE
  | TIMES
  | THEN
  | SKIP
  | SEQUENCE
  | RPAREN
  | PLUS
  | OUTPUT
  | OR
  | NOT
  | MODULO
  | MINUS
  | MAIN
  | LT
  | LPAREN
  | LE
  | INT of (int)
  | INPUT
  | IF
  | IDENT of (string)
  | GT
  | GE
  | EQ
  | EOF
  | ELSE
  | DO
  | DIVIDE
  | DEF
  | BOOL of (bool)
  | ASSIGN
  | AS
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (MiniImp.program)
