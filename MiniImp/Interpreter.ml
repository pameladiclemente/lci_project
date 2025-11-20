(* Project Fragment: 
Write a pair of interpreters (ocaml programs), one for MiniImp
and one for MiniFun/MiniTyFun that:
• Read a MiniImp/Fun program passed as a parameter
• Read an integer input for the MiniImp/Fun program, passed via
standard input
• Evaluate the program given the input and print the resulting
integer in standard output
*)

open MiniImp
open Lexer
exception ParsingError of string

(* Transform parser tokens into readable strings *)
let string_of_token = function
  | Parser.INT n -> Printf.sprintf "INT(%d)" n
  | Parser.BOOL b -> Printf.sprintf "BOOL(%b)" b
  | Parser.IDENT s -> Printf.sprintf "IDENT(%s)" s
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.TIMES -> "TIMES"
  | Parser.DIVIDE -> "DIVIDE"
  | Parser.MODULO -> "MODULO"
  | Parser.LT -> "LT"
  | Parser.LE -> "LE"
  | Parser.GT -> "GT"
  | Parser.GE -> "GE"
  | Parser.EQ -> "EQ"
  | Parser.AND -> "AND"
  | Parser.OR -> "OR"
  | Parser.NOT -> "NOT"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.DEF -> "DEF"
  | Parser.MAIN -> "MAIN"
  | Parser.WITH -> "WITH"
  | Parser.INPUT -> "INPUT"
  | Parser.OUTPUT -> "OUTPUT"
  | Parser.AS -> "AS"
  | Parser.SKIP -> "SKIP"
  | Parser.IF -> "IF"
  | Parser.THEN -> "THEN"
  | Parser.ELSE -> "ELSE"
  | Parser.WHILE -> "WHILE"
  | Parser.DO -> "DO"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.SEQUENCE -> "SEQUENCE"
  | Parser.EOF -> "EOF"

(* Read input file and return MiniImp program as string *)
let read_file filename =
  let channel = open_in filename in
  let length = in_channel_length channel in
  let text = really_input_string channel length in
  close_in channel;
  text

(* Parse a MiniImp program *)
let parse_miniimp input =
  let lexbuf = Lexing.from_string input in
  let read_tokens = ref [] in  
  
  (* Store parsed tokens *)
  let rec lex_and_store () =
    let token = Lexer.read lexbuf in
    read_tokens := !read_tokens @ [token]; 
    if token = Parser.EOF then token else lex_and_store ()
  in

  (* Print read tokens before parsing *)
  (try
     ignore (lex_and_store ());
     List.iter (fun t -> Printf.printf "%s " (string_of_token t)) !read_tokens;
     Printf.printf "\n"
   with _ -> Printf.printf "(Error reading tokens)\n");

  (* Perform parsing; if error occurs, print tokens read before error *)
  let lexbuf = Lexing.from_string input in
  try
    Parser.program Lexer.read lexbuf
  with
  | Lexer.LexingError msg ->
      let pos = lexbuf.lex_curr_p in
      failwith (Printf.sprintf "Lexer error: %s at line %d, column %d"
                  msg pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1))
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let token = Lexing.lexeme lexbuf in
      Printf.printf "Token until errors: ";
      List.iter (fun t -> Printf.printf "%s " (string_of_token t)) !read_tokens;
      Printf.printf "\n";
      failwith (Printf.sprintf "Parsing error near '%s' at line %d, column %d"
                  token pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1))

(* Main function: 
read program file, parse it, read input value, evaluate program and print output value 
*)
let main () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Employing: %s <file_miniimp>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let program_text = read_file filename in
  let parsed_program = parse_miniimp program_text in

  Printf.printf "Input: ";
  let input_value = read_int () in
  let result = eval_program parsed_program input_value in
  Printf.printf "Output: %d\n" result

(*let () = main ()*)
