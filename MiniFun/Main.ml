open MiniFun
open Parser
open Lexer

exception ParseError of string

(* Funzione per fare il parsing di un file contenente un programma MiniFun *)
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

(* Funzione per interpretare un programma MiniFun *)
let interpreter filename =
  try
    (* Parsing del programma *)
    let program = parse_program filename in

    (* Chiedere l'input all'utente *)
    print_string "Enter input value: ";
    let input = read_int () in

    (* Ambiente iniziale: mappa che associa l'input alla variabile "in" *)
    let initial_env = StringMap.singleton "in" (MemInteger input) in

    (* Valutare il programma *)
    let result = eval_term program initial_env in

    (* Stampare il risultato *)
    match result with
    | MemInteger n -> Printf.printf "Output: %d\n" n
    | MemBoolean b -> Printf.printf "Output: %b\n" b
    | Closure _ | RecClosure _ ->
        Printf.printf "Output: Function value (closure) cannot be displayed\n"
  with
  | ParseError msg -> Printf.eprintf "Error: %s\n" msg
  | Failure msg -> Printf.eprintf "Runtime error: %s\n" msg

(* Punto di ingresso principale *)
let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    interpreter filename
