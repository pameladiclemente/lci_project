open MiniImp
open CFG
open MiniRISC
open Parser
open LivenessAnalysis
open Lexing
 
(* Function to format position for error messages.
   It converts a Lexing position into a readable string "line X, column Y". *)
let string_of_pos pos =
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "at line %d, column %d" line col

(* Helper function to parse a MiniImp source file.
  It handles opening the file, running the lexer and parser, and catching errors. *)
let parse_file filename =
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  try
    let program = Parser.program Lexer.read lexbuf in
    close_in input_channel;
    program
  with
  | Lexer.LexingError msg ->
      Printf.eprintf "Lexical Error %s: %s\n" (lexeme_start_p lexbuf |> string_of_pos) msg;
      exit 1
  | Parser.Error ->
      Printf.eprintf "Syntax Error %s\n" (lexeme_start_p lexbuf |> string_of_pos);
      exit 1
  | e ->
      close_in_noerr input_channel;
      Printf.eprintf "Unexpected error during parsing: %s\n" (Printexc.to_string e);
      exit 1

(* Function containing the entire compilation pipeline, from MiniImp source to MiniRISC assembly *)
let compile_miniimp_to_minirisc (n : int) (input_file : string) (output_file : string) 
                                (def_var_flag : bool) (optimize_flag : bool) =
  
  Printf.printf "Starting compilation of '%s'...\n" input_file;

  (* 1. Parse the input file into an AST *)
  let program = parse_file input_file in
  
  (* Extract input/output variable names from the AST (for later use in analysis) *)
  let input_var = match program with Program (input, _, _) -> input in
  let output_var = match program with Program (_, output, _) -> output in

  (* 2. Build the Control Flow Graph (CFG) from the AST. *)
  let cfg = build_cfg program in

  (* 3. Static Analysis: Check for undefined variables (if enabled).
     This ensures that variables are assigned a value before being used. *)
  if def_var_flag then (
    Printf.printf "Running static analysis (defined variables)...\n";
    if not (defined_variables cfg input_var) then (
      Printf.eprintf "Error: The program uses variables that may not be initialized\n";
      exit 1
    ) else (
      Printf.printf "Variable initialization check passed.\n"
    )
  );

  (* 4. Translate MiniImp CFG to MiniRISC CFG assuming an infinite number of virtual registers. *)
  let risc_cfg = impcfg_in_riscfg cfg in

  (* 5. Optimization and Target Code Generation.
     We handle two main cases: with or without optimization. *)
  let final_cfg =
    if optimize_flag then (
      (* Phase 5A: Register Optimization via Graph Coloring. *)
      Printf.printf "Optimization enabled. Running Liveness Analysis and Register Allocation...\n";
      let optimized_cfg = optimize_registers risc_cfg output_var in
      
      (* Phase 5B: Target Code Generation. *)
      Printf.printf "Generating target code for %d registers...\n" n;
      translate_to_target optimized_cfg n
      
    ) else (
      (* Optimization disabled: 
      Skip Graph Coloring but must still perform Target Code Generation *)
      Printf.printf "Optimization disabled. Generating target code directly for %d registers...\n" n;
      translate_to_target risc_cfg n
    )
  in

  (* 6. Convert the final RISC CFG into a string representation of the assembly code. *)
  let risc_code = riscfg_in_assembly final_cfg in
  
  (* Write the generated assembly to the specified output file *)
  let out_channel = open_out output_file in
  output_string out_channel risc_code;
  close_out out_channel;

  Printf.printf "Compilation successful! MiniRISC code written to: %s\n" output_file

(* Entry point *)
let () =
  (* Read command line arguments *)
  let argv = Sys.argv in
  let argc = Array.length argv in

  (* Check if we have the exact required arguments (5 user args + program name = 6).
     Usage: ./compiler <num_registers> <input_file> <output_file> <check_undef_bool> <optimize_bool> *)
  if argc <> 6 then (
    Printf.eprintf "Usage: %s <num_registers> <input_file> <output_file> <check_undef_bool> <optimize_bool>\n" argv.(0);
    Printf.eprintf "Example: %s 8 prog.miniimp out.asm true false\n" argv.(0);
    exit 1
  );

  (* Parse mandatory argument: Number of Registers *)
  let registers_number = 
    try int_of_string argv.(1) 
    with Failure _ -> 
      Printf.eprintf "Error: Number of registers must be an integer.\n"; 
      exit 1 
  in
  
  (* Ensure minimum register count requirement (this is a double check) *)
  if registers_number < 4 then (
    Printf.eprintf "Error: Target architecture must have at least 4 registers.\n";
    exit 1
  );

  (* Parse file paths *)
  let input_file = argv.(2) in
  let output_file = argv.(3) in

  (* Parse boolean flags directly *)
  let def_var_flag = 
    try bool_of_string argv.(4)
    with Invalid_argument _ ->
      Printf.eprintf "Error: check_undef_bool must be 'true' or 'false'.\n";
      exit 1
  in

  let optimize_flag = 
    try bool_of_string argv.(5)
    with Invalid_argument _ ->
      Printf.eprintf "Error: optimize_bool must be 'true' or 'false'.\n";
      exit 1
  in

  (* Execute the compilation pipeline *)
  try
    compile_miniimp_to_minirisc registers_number input_file output_file def_var_flag optimize_flag
  with
  | Failure msg -> 
      Printf.eprintf "Fatal Error: %s\n" msg;
      exit 1
  | e -> 
      Printf.eprintf "Unhandled Exception: %s\n" (Printexc.to_string e);
      exit 1