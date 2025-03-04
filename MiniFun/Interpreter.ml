open MiniFun
open Lexing

exception ParsingError of string

(* Converti i token del parser in stringhe leggibili *)
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
  | Parser.IF -> "IF"
  | Parser.THEN -> "THEN"
  | Parser.ELSE -> "ELSE"
  | Parser.LET -> "LET"
  | Parser.LETFUN -> "LETFUN"
  | Parser.FUN -> "FUN"
  | Parser.IN -> "IN"
  | Parser.ARROW -> "ARROW"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.EOF -> "EOF"
  | Parser.LPAREN -> "("
  | Parser.RPAREN -> ")"

(* Legge il file di input e restituisce il programma MiniFun come stringa *)
let read_file filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let text = really_input_string chan len in
  close_in chan;
  text

(* Effettua il parsing di un programma MiniFun *)
let parse_minifun input =
  let lexbuf = Lexing.from_string input in
  let read_tokens = ref [] in  

  (* Memorizza i token letti *)
  let rec lex_and_store () =
    let token = Lexer.read lexbuf in
    read_tokens := !read_tokens @ [token]; 
    if token = Parser.EOF then token else lex_and_store ()
  in

  (* Stampa i token letti prima del parsing *)
  (try
     ignore (lex_and_store ());
     List.iter (fun t -> Printf.printf "%s " (string_of_token t)) !read_tokens;
     Printf.printf "\n"
   with _ -> Printf.printf "(Errore nella lettura dei token)\n");

  (* Reset del buffer per rileggere il file e analizzarlo con il parser *)
  let lexbuf = Lexing.from_string input in
  try
    Parser.program Lexer.read lexbuf
  with
  | Lexer.LexingError msg ->
      let pos = lexbuf.lex_curr_p in
      failwith (Printf.sprintf "Errore lexer: %s line %d, column %d"
                  msg pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1))
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let token = Lexing.lexeme lexbuf in
      Printf.printf "Token read until error: ";
      List.iter (fun t -> Printf.printf "%s " (string_of_token t)) !read_tokens;
      Printf.printf "\n";
      failwith (Printf.sprintf "Parsing error near '%s' line %d, column %d"
                  token pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1))

(* Funzione principale dell'interprete *)
let main () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Token employed: %s <file_minifun>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let program_text = read_file filename in
  let parsed_program = parse_minifun program_text in

  (* Legge un numero da input e lo inserisce nell'ambiente iniziale *)
  Printf.printf "Input: ";
  let input_value = read_int () in
  let initial_env = MiniFun.StringMap.add "input" (MiniFun.MemInteger input_value) MiniFun.StringMap.empty in

  (* Esegue il programma MiniFun *)
  let result = MiniFun.eval_term parsed_program initial_env in

  (* Stampa il risultato *)
  match result with
  | MiniFun.MemInteger n -> Printf.printf "Output: %d\n" n
  | MiniFun.MemBoolean b -> Printf.printf "Output: %b\n" b
  | _ -> failwith "Error: output not valid"

(* Avvia il programma *)
let () = main ()