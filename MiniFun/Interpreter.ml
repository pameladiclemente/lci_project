(* Project Fragment:
Write a pair of interpreters (ocaml programs), one for MiniImp
and one for MiniFun/MiniTyFun that:
• Read a MiniImp/Fun program passed as a parameter
• Read an integer input for the MiniImp/Fun program, passed via
standard input
• Evaluate the program given the input and print the resulting
integer in standard output
*)

open MiniFun
open Lexing
open Lexer
open Parser

exception ParsingError of string

(* Transform parser tokens into readable strings *)
let string_of_token = function
  | INT n -> Printf.sprintf "INT(%d)" n
  | BOOL b -> Printf.sprintf "BOOL(%b)" b
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVIDE -> "DIVIDE"
  | MODULO -> "MODULO"
  | LT -> "LT"
  | LE -> "LE"
  | GT -> "GT"
  | GE -> "GE"
  | EQ -> "EQ"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | LET -> "LET"
  | LETFUN -> "LETFUN"
  | FUN -> "FUN"
  | IN -> "IN"
  | ARROW -> "ARROW"
  | ASSIGN -> "ASSIGN"
  | EOF -> "EOF"
  | LPAREN -> "("
  | RPAREN -> ")"

(* Read input file and return MiniFun program as string *)
let read_file filename =
  let channel = open_in filename in
  let length = in_channel_length channel in
  let text = really_input_string channel length in
  close_in channel;
  text

(* Parse a MiniFun program *)
let parse_minifun input =
  let lexbuf = from_string input in
  let read_tokens = ref [] in

  (* Store parsed tokens *)
  let rec lex_and_store () =
    let token = read lexbuf in
    read_tokens := !read_tokens @ [ token ];
    if token = EOF then token else lex_and_store ()
  in

  (* Print read tokens before parsing *)
  (try
     ignore (lex_and_store ());
     List.iter (fun t -> Printf.printf "%s " (string_of_token t)) !read_tokens;
     Printf.printf "\n"
   with _ -> Printf.printf "(Errore nella lettura dei token)\n");

  (* Perform parsing; if error occurs, print tokens read before error *)
  let lexbuf = from_string input in
  try program read lexbuf with
  | LexingError msg ->
      let pos = lexbuf.lex_curr_p in
      failwith
        (Printf.sprintf "Errore lexer: %s line %d, column %d" msg pos.pos_lnum
           (pos.pos_cnum - pos.pos_bol + 1))
  | Error ->
      let pos = lexbuf.lex_curr_p in
      let token = lexeme lexbuf in
      Printf.printf "Token read until error: ";
      List.iter (fun t -> Printf.printf "%s " (string_of_token t)) !read_tokens;
      Printf.printf "\n";
      failwith
        (Printf.sprintf "Parsing error near '%s' line %d, column %d" token
           pos.pos_lnum
           (pos.pos_cnum - pos.pos_bol + 1))

(* Main function: 
read program file, parse it, read input value, evaluate program and print output value 
*)
let main () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Token employed: %s <file_minifun>\n" Sys.argv.(0);
    exit 1);

  let filename = Sys.argv.(1) in
  let program_text = read_file filename in
  let parsed_program = parse_minifun program_text in

  Printf.printf "Input: ";
  let input_value = read_int () in
  let initial_env =
    StringMap.add "input" (MemInteger input_value) StringMap.empty
  in

  let result = eval_term parsed_program initial_env in

  match result with
  | MemInteger n -> Printf.printf "Output: %d\n" n
  | MemBoolean b -> Printf.printf "Output: %b\n" b
  | _ -> failwith "Error: output not valid"

let () = main ()
