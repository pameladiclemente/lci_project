(* Project fragment:
- Extend the concrete syntax with parenthesis for forcing the evaluation order 
(no need to change the abstract syntax)
- Define lexers and parsers for MiniImp and MiniFun (or MiniTyFun, as you prefer) 
by using ocamllex and menhir
- Get rid of ambiguities: menhir should not produce warnings!
*)

{
  open Parser
  exception LexingError of string
}

(* Regular expressions for... *)
let integer = ['0'-'9']['0'-'9']* (* ... integer numbers *)
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*  (* ... identifiers *)
let white = [' ' '\t']+ | '\r' | '\n' | "\r\n"  (* ... whitespace *)
let line_comment = "//" [^ '\n']* (* ...line comments (//) *)

(* Lexing rules *)
rule read = parse
| white        { read lexbuf }  
| line_comment { read lexbuf }
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
| "("          { LPAREN } 
| ")"          { RPAREN } 
| eof          { EOF } 
| ident        { 
    match Lexing.lexeme lexbuf with
    | "true"     -> BOOL(true)
    | "false"    -> BOOL(false)
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

