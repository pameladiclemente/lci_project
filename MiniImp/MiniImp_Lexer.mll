(* Defined lexer for MiniImp that supports parentheses for evaluation order. *)
{
  open Parser
  exception LexingError of string
}

(* Regular expressions for... *)
let integer = ['0'-'9']+  (* ... integer numbers *)
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*  (* ... identifiers *)
let white = [' ' '\t']+ | '\r' | '\n' | "\r\n"  (* ... whitespace *)

(* Lexing rules *)
rule read = parse
| white        { read lexbuf }  (* Skip whitespace *)
| integer      { INT(int_of_string (Lexing.lexeme lexbuf)) } 
| ident        { 
    match Lexing.lexeme lexbuf with
    | "def"      -> DEF
    | "input"    -> INPUT
    | "output"   -> OUTPUT
    | "as"       -> AS
    | "skip"     -> SKIP
    | "if"       -> IF
    | "then"     -> THEN
    | "else"     -> ELSE
    | "while"    -> WHILE
    | "do"       -> DO
    | "true"     -> TRUE
    | "false"    -> FALSE
    | _          -> IDENT(Lexing.lexeme lexbuf)  (* Identifiers *)
  }
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
| "and"        { AND }
| "or"         { OR }
| "not"        { NOT }
| ":="         { ASSIGN }
| ";"          { SEMICOLON }
| "("          { LPAREN } (* Left parethesis (opening one) *)
| ")"          { RPAREN } (* Right parethesis (closing one)  *)
| eof          { EOF } (* End of file *)
| _            { raise (LexingError ("Unknown token: " ^ Lexing.lexeme lexbuf)) }




{
  open Parser 
  exception LexingError of string

  (* Keywords in the MiniImp language *)
  let tokens = [
    ("skip", SKIP);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("while", WHILE);
    ("do", DO);
    ("true", BOOL true);
    ("false", BOOL false);
    ("and", AND);
    ("not", NOT);
  ]
}

(* Regular Expressions for MiniImp tokens *)
let integer = ['0'-'9']+                          (* Integer literals: Matches sequences of digits ('0'-'9') and converts them to integers *)
let variable = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* (* Identifiers: Matches identifiers (variable names) starting with a letter and followed by alphanumeric characters or underscores. *)
let white = [' ' '\t']+ | '\r' | '\n'             (* Whitespace: Matches and skips whitespace (spaces, tabs, and newlines). *)

(* Lexing rules *)
rule read = parse
  | white { read lexbuf }                         (* Skip whitespace by recursively calling read *)
  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) } (* Integer tokens recognized as INT tokens*)
  | variable as var ->                                (* Check for tokens or identifiers: Checked against the keywords list. If found, the corresponding token is emitted; otherwise, it's an identifier (IDENT). *)
      (try List.assoc var tokens with Not_found -> IDENT var)
      (* Operators and Symbols: Specific symbols like +, -, *, <, :=, ;, (, and ) are directly mapped to their respective tokens. *)
  | "+" { PLUS }                                  (* Addition operator *)
  | "-" { MINUS }                                 (* Subtraction operator *)
  | "*" { TIMES }                                 (* Multiplication operator *)
  | "<" { LT }                                    (* Less-than operator *)
  | ":=" { ASSIGN }                               (* Assignment operator *)
  | ";" { SEMICOLON }                             (* Statement separator *)
  | "(" { L_PAREN }                                (* Left parenthesis *)
  | ")" { R_PAREN }                                (* Right parenthesis *)
  | eof { EOF }                                   (* End of file *)
  | _ as char { raise (LexingError (Printf.sprintf "Unexpected character: %c" char)) }

(*
Notes for Integration
Parser Tokens: Ensure that the token names (e.g., PLUS, MINUS, LPAREN) match those declared in your parser's .mly file.
Lexer Entry Point: Compile the lexer with ocamllex (e.g., ocamllex mylexer.mll) to generate mylexer.ml. The read function is the lexer entry point.
Testing: Test the lexer by feeding it MiniImp programs and verifying the generated tokens using OCaml's REPL or debugging tools.
This lexer is now fully functional for MiniImp and supports parentheses for evaluation order.
*)