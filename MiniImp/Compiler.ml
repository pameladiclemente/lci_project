open MiniImp
open CFG
open MiniRISC
open Parser
open LivenessAnalysis (* <-- AGGIUNTO OPEN *)
open Lexing

(* Funzione helper per parsare un file *)
let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let prog = Parser.program Lexer.read lexbuf in
    close_in ic;
    prog
  with
  | Lexer.LexingError msg ->
      Printf.eprintf "Errore lessicale %s: %s\n" (lexeme_start_p lexbuf |> string_of_pos) msg;
      exit 1
  | Parser.Error ->
      Printf.eprintf "Errore sintattico %s\n" (lexeme_start_p lexbuf |> string_of_pos);
      exit 1

(* Funzione principale del compilatore *)
let compile_miniimp_to_minirisc (n : int) (input_file : string) (output_file : string) 
                                (check_undef : bool) (optimize : bool) =
  (* 1️⃣ Leggiamo il file MiniImp e lo trasformiamo in un AST *)
  let program = parse_file input_file in
  let input_var = match program with Program (input, _, _) -> input in
  let output_var = match program with Program (_, output, _) -> output in

  (* 2️⃣ Costruiamo il CFG di MiniImp *)
  let cfg = build_cfg program in

  (* 3️⃣ Controllo delle variabili non inizializzate (se attivato) *)
  if check_undef then
    if not (defined_variables cfg input_var) then
      failwith "Errore: il programma MiniImp usa variabili non definite!"
    else
      Printf.printf "✅ Controllo variabili superato!\n";

  (* 4️⃣ Convertiamo il CFG MiniImp in CFG MiniRISC *)
  let risc_cfg = impcfg_in_riscfg cfg in

  (* 5️⃣ Ottimizziamo il CFG MiniRISC (se attivato) *)
  let optimized_cfg =
    if optimize then (
      Printf.printf "⚡ Ottimizzazione attivata...\n";
      
      (* --- MODIFICATO --- *)
      (* Fase A: FUSIONE REGISTRI (Slide 206-210) *)
      let merged_cfg = optimize_registers risc_cfg output_var in
      
      (* Fase B: RIVERSAMENTO REGISTRI (Spilling) (Slide 201-204) *)
      Printf.printf "  [Opt] Avvio riversamento (spilling) per %d registri fisici...\n" n;
      translate_to_target merged_cfg n
      
    ) else risc_cfg
  in

  (* 6️⃣ Generiamo il codice MiniRISC e lo scriviamo su file *)
  let risc_code = riscfg_in_assembly optimized_cfg in
  let oc = open_out output_file in
  output_string oc risc_code;
  close_out oc;

  Printf.printf "✅ Compilazione completata! Il codice MiniRISC è stato salvato in: %s\n" output_file