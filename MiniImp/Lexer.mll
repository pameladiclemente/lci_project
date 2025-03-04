(* Defined lexer for MiniImp that supports parentheses for evaluation order. *)
{
  open Parser
  exception LexingError of string
}

(* Regular expressions for... *)
let digit = ['0'-'9'] (* ... digit *)
let integer = '-'?['0'-'9']['0'-'9']* (* ... integer numbers *)
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*  (* ... identifiers *)
let white = [' ' '\t']+ | '\r' | '\n' | "\r\n"  (* ... whitespace *)
let line_comment = "//" [^ '\n']* (* ...line comments (//) *)

(* Lexing rules *)
rule read = parse
| white        { read lexbuf }  (* Skip whitespace *)
| line_comment { read lexbuf }
| digit        { INT(int_of_string (Lexing.lexeme lexbuf))}
| integer      { INT(int_of_string (Lexing.lexeme lexbuf)) } 
| "+"          { PLUS } 
| "-"          { MINUS }
| "*"          { TIMES }
| "/"          { DIVIDE }
| "%"          { MODULO }
| "<"          { LT }
| "<="         { LE }
| ">"          { GT }
| ">="         { GE }
| "=="         { EQ }
| "&&"         { AND }
| "||"         { OR }
| ":="         { ASSIGN }
| ";"          { SEQUENCE }
| "("          { LPAREN } (* Left parethesis (opening one) *)
| ")"          { RPAREN } (* Right parethesis (closing one) *)
| eof          { EOF } (* End of file *)
| ident        { 
    match Lexing.lexeme lexbuf with
    | "def"      -> DEF
    | "main"     -> MAIN
    | "with"     -> WITH
    | "input"    -> INPUT
    | "output"   -> OUTPUT
    | "as"       -> AS
    | "skip"     -> SKIP
    | "if"       -> IF
    | "then"     -> THEN
    | "else"     -> ELSE
    | "while"    -> WHILE
    | "do"       -> DO
    | "not"      -> NOT
    | _          -> IDENT(Lexing.lexeme lexbuf)  (* Identifiers *)
  }
| _            { raise (LexingError ("Unknown token: " ^ Lexing.lexeme lexbuf)) }

(*
Notes for Integration
Parser Tokens: Ensure that the token names (e.g., PLUS, MINUS, LPAREN) match those declared in your parser's .mly file.
Lexer Entry Point: Compile the lexer with ocamllex (e.g., ocamllex mylexer.mll) to generate mylexer.ml. The read function is the lexer entry point.
Testing: Test the lexer by feeding it MiniImp programs and verifying the generated tokens using OCaml's REPL or debugging tools.
This lexer is now fully functional for MiniImp and supports parentheses for evaluation order.
*)