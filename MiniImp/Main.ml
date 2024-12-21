open MiniImp
open Parser
open Lexer

exception ParseError of string

(* Function to parse a program file *)
let parse_program filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  try
    let program = Parser.program Lexer.read lexbuf in
    close_in channel;
    program
  with
  | Lexer.LexingError msg ->
      close_in channel;
      raise (ParseError ("Lexing error: " ^ msg))
  | Parsing.Parse_error ->
      close_in channel;
      raise (ParseError "Parsing error")

(* Function to interpret a MiniImp program *)
let interpreter filename =
  try
    (* Parse the program *)
    let program = parse_program filename in

    (* Ask the user for input *)
    print_string "Enter input value: ";
    let input = read_int () in

    (* Evaluate the program *)
    let result = eval_program program input in

    (* Print the output *)
    Printf.printf "Output: %d\n" result
  with
  | ParseError msg -> Printf.eprintf "Error: %s\n" msg
  | Failure msg -> Printf.eprintf "Runtime error: %s\n" msg

(* Entry point *)
let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    interpreter filename
