(* Testing MiniTyFun.ml *)

open MiniTyFun
open Printf

(* Helper function to write clear messages *)
let rec string_of_type = function
  | IntType -> "int"
  | BoolType -> "bool"
  | FunType (t1, t2) ->
      sprintf "(%s -> %s)" (string_of_type t1) (string_of_type t2)

(* IMPORTANT!
Since we don't have a parser for MiniTyFun, we will manually construct terms to test the type checker.
Below are some example terms and their expected types.*)

let test_program_1 () =
  (* Example: fun x:int => x + 1 *)
  (* Corresponds to: Function ("x", IntType, Add(Variable "x", Integer 1)) *)
  let term = Function ("x", IntType, Add (Variable "x", Integer 1)) in

  printf "Testing: fun x:int => x + 1\n";
  match check_type StringMap.empty term with
  | Some t ->
      printf "Type Check completed. Result Type: %s\n" (string_of_type t)
  | None -> printf "Type Check Failed.\n"

let test_program_2 () =
  (* Example (Error): fun x:int => x + true *)
  (* Corresponds to: Function ("x", IntType, Add(Variable "x", Boolean true)) *)
  let term = Function ("x", IntType, Add (Variable "x", Boolean true)) in

  printf "\nTesting (Error Case): fun x:int => x + true\n";
  match check_type StringMap.empty term with
  | Some t ->
      printf "Type Check completed. Result Type: %s\n" (string_of_type t)
  | None -> printf "Type Check Failed. \n"

let test_program_3 () =
  (* Example: letfun f (x:int) : int = x + 1 in f 5 *)
  (* Corresponds to: LetFun("f", "x", IntType, IntType, Add(Var "x", Int 1), FunApp(Var "f", Int 5)) *)
  let term =
    LetFun
      ( "f",
        "x",
        IntType,
        IntType,
        Add (Variable "x", Integer 1),
        FunctionApplication (Variable "f", Integer 5) )
  in

  printf "\nTesting: letfun f (x:int) : int = x + 1 in f 5\n";
  match check_type StringMap.empty term with
  | Some t ->
      printf "Type Check completed. Result Type: %s\n" (string_of_type t)
  | None -> printf "Type Check Failed.\n"

let () =
  test_program_1 ();
  test_program_2 ();
  test_program_3 ()
