open Printf
open MiniImp
open Lexer
open Parser

(* A test function to parse, evaluate, and print results *)
let test_program input input_value =
  printf "Input program: %s\n" input;

  (* Parse the program *)
  let lexbuf = Lexing.from_string input in
  let program = Parser.program Lexer.read lexbuf in
  printf "Parsed program: %s\n" (match program with
    | Program (input, output, cmd) ->
        sprintf "Program(input=%s, output=%s, ...)" input output);

  (* Evaluate the program *)
  let result = MiniImp.eval_program program input_value in
  printf "Result: %d\n" result

(* Main entry point for testing *)
let () =
  let test_input = "DEF MAIN WITH INPUT x = 5 OUTPUT y AS z = Skip EOF" in
  let input_value = 5 in
  test_program test_input input_value
