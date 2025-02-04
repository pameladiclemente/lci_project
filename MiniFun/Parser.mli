(* ESEGUENDO IL TUTTO IN TEORIA SI LEVANO GLI ERRORI

parser.mli *)

(* Il modulo MiniImp è aperto per accedere ai tipi definiti in esso *)
open MiniFun

(* Funzione generata dal parser per analizzare un programma completo *)
val program : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> program
