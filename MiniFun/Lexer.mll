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
| white               { read lexbuf }  (* Skip whitespace *)
| line_comment        { read lexbuf }
| integer             { INT(int_of_string (Lexing.lexeme lexbuf)) } 
| "=>"                { ARROW }
| "="                 { ASSIGN }
| "+"                 { PLUS } 
| "-"                 { MINUS }
| "*"                 { TIMES }
| "/"                 { DIVIDE }
| "%"                 { MODULO }
| "<"                 { LT }
| "<="                { LE }
| ">"                 { GT }
| ">="                { GE }
| "=="                { EQ }
| "&&"                { AND }
| "||"                { OR }
| "("                 { LPAREN }
| ")"                 { RPAREN }

| eof                 { EOF } 
| ident               { 
      match Lexing.lexeme lexbuf with
      | "true"     -> BOOL(true)
      | "false"    -> BOOL(false)
      | "let"      -> LET 
      | "letfun"   -> LETFUN 
      | "fun"      -> FUN 
      | "if"       -> IF
      | "then"     -> THEN
      | "else"     -> ELSE
      | "in"       -> IN
      | "not"      -> NOT
      | _          -> IDENT(Lexing.lexeme lexbuf)  (* Identifiers *)
  } 
| _                   { raise (LexingError ("Unknown token: " ^ Lexing.lexeme lexbuf)) }
