open MiniImp
open CFG
open MiniRISC
open Parser
open LivenessAnalysis
open Lexing

(* ------------------------------------------------------------------------- *)
(* Helper Functions for Parsing and Error Reporting                          *)
(* ------------------------------------------------------------------------- *)

(* Function to format position for error messages.
   It converts a Lexing position into a readable string "line X, column Y". *)
let string_of_pos pos =
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "at line %d, column %d" line col

(* Helper function to parse a MiniImp source file into an Abstract Syntax Tree (AST).
   It handles opening the file, running the lexer and parser, and catching errors. *)
let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  (* Set the filename in the lexbuf for better error messages *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let prog = Parser.program Lexer.read lexbuf in
    close_in ic;
    prog
  with
  | Lexer.LexingError msg ->
      Printf.eprintf "Lexical Error %s: %s\n" (lexeme_start_p lexbuf |> string_of_pos) msg;
      exit 1
  | Parser.Error ->
      Printf.eprintf "Syntax Error %s\n" (lexeme_start_p lexbuf |> string_of_pos);
      exit 1
  (* Catch-all for any other unexpected errors during parsing (e.g., I/O errors) *)
  | e ->
      close_in_noerr ic;
      Printf.eprintf "Unexpected error during parsing: %s\n" (Printexc.to_string e);
      exit 1

(* ------------------------------------------------------------------------- *)
(* Core Compilation Logic                                                    *)
(* ------------------------------------------------------------------------- *)

(* This function orchestrates the entire compilation pipeline:
   1. Parse MiniImp code -> AST
   2. Build Control Flow Graph (CFG)
   3. (Optional) Static Analysis for undefined variables
   4. Translate MiniImp CFG -> MiniRISC CFG
   5. (Optional) Optimization via Graph Coloring
   6. Register Spilling (Target Code Gen)
   7. Emit final Assembly code to file
*)
let compile_miniimp_to_minirisc (n : int) (input_file : string) (output_file : string) 
                                (check_undef : bool) (optimize : bool) =
  
  Printf.printf "Starting compilation of '%s'...\n" input_file;

  (* 1. Parse the input file into an AST *)
  let program = parse_file input_file in
  
  (* Extract input/output variable names from the AST for later use in analysis *)
  let input_var = match program with Program (input, _, _) -> input in
  let output_var = match program with Program (_, output, _) -> output in

  (* 2. Build the Control Flow Graph (CFG) from the AST.
     The CFG represents the program logic as connected blocks of simple commands. *)
  let cfg = build_cfg program in

  (* 3. Static Analysis: Check for undefined variables (if enabled).
     This ensures that variables are assigned a value before being used. *)
  if check_undef then (
    Printf.printf "Running static analysis (defined variables)...\n";
    if not (defined_variables cfg input_var) then (
      Printf.eprintf "Error: The program uses variables that may not be initialized!\n";
      exit 1
    ) else (
      Printf.printf "Variable initialization check passed.\n"
    )
  );

  (* 4. Translate MiniImp CFG to MiniRISC CFG.
     This step converts high-level MiniImp constructs into low-level MiniRISC instructions,
     but still assumes an infinite number of virtual registers. *)
  let risc_cfg = impcfg_in_riscfg cfg in

  (* 5. Optimization and Target Code Generation.
     We handle two main cases: with or without optimization. *)
  let final_cfg =
    if optimize then (
      Printf.printf "Optimization enabled. Running Liveness Analysis and Register Allocation...\n";
      
      (* Phase A: Register Optimization via Graph Coloring.
         This tries to merge non-interfering virtual registers to reduce the total count. *)
      let optimized_cfg = optimize_registers risc_cfg output_var in
      
      (* Phase B: Target Code Generation with Spilling.
         This maps the (potentially reduced) virtual registers to 'n' physical registers.
         If 'n' is not enough, it automatically spills excess registers to memory. *)
      Printf.printf "Generating target code for %d registers...\n" n;
      translate_to_target optimized_cfg n
      
    ) else (
      (* Optimization disabled: Skip Graph Coloring but MUST still perform Spilling
         to ensure the code works on the target architecture with 'n' registers. *)
      Printf.printf "Optimization disabled. Generating target code directly for %d registers...\n" n;
      translate_to_target risc_cfg n
    )
  in

  (* 6. Emit Assembly Code.
     Convert the final RISC CFG into a string representation of the assembly code. *)
  let risc_code = riscfg_in_assembly final_cfg in
  
  (* Write the generated assembly to the specified output file *)
  let oc = open_out output_file in
  output_string oc risc_code;
  close_out oc;

  Printf.printf "Compilation successful! MiniRISC code written to: %s\n" output_file


(* ------------------------------------------------------------------------- *)
(* Main Entry Point (Command Line Argument Parsing)                          *)
(* ------------------------------------------------------------------------- *)

let () =
  (* Read command line arguments *)
  let argv = Sys.argv in
  let argc = Array.length argv in

  (* Check if we have the minimum required arguments.
     Usage: ./compiler <num_registers> <input_file> <output_file> [options] *)
  if argc < 4 then (
    Printf.eprintf "Usage: %s <num_registers> <input_file> <output_file> [options]\n" argv.(0);
    Printf.eprintf "Options:\n";
    Printf.eprintf "  -no-check   Disable undefined variable check\n";
    Printf.eprintf "  -opt        Enable register optimization (Graph Coloring)\n";
    exit 1
  );

  (* Parse mandatory argument: Number of Registers *)
  let n_regs = 
    try int_of_string argv.(1) 
    with Failure _ -> 
      Printf.eprintf "Error: Number of registers must be an integer.\n"; 
      exit 1 
  in
  
  (* Ensure minimum register count requirement (needed for spilling logic) *)
  if n_regs < 4 then (
    Printf.eprintf "Error: Target architecture must have at least 4 registers.\n";
    exit 1
  );

  (* Parse mandatory arguments: File paths *)
  let input_file = argv.(2) in
  let output_file = argv.(3) in

  (* Parse optional flags *)
  let check_undef = ref true in (* Default: Check is Enabled *)
  let optimize = ref false in   (* Default: Optimization is Disabled *)

  (* Loop through remaining arguments to find flags *)
  for i = 4 to argc - 1 do
    match argv.(i) with
    | "-no-check" -> check_undef := false
    | "-opt" -> optimize := true
    | arg -> Printf.eprintf "Warning: Unknown option '%s' ignored.\n" arg
  done;

  (* Execute the compilation pipeline *)
  try
    compile_miniimp_to_minirisc n_regs input_file output_file !check_undef !optimize
  with
  | Failure msg -> 
      Printf.eprintf "Fatal Error: %s\n" msg;
      exit 1
  | e -> 
      Printf.eprintf "Unhandled Exception: %s\n" (Printexc.to_string e);
      exit 1