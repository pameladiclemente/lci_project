(* Project fragment 1: 
Create a module for MiniImp that exposes the type of the abstract syntax tree and an evaluation function. *)

(* MiniImp syntax 
 Arithmetic expressions; division, modulo and negative numbers have been included *)
type a_exp =
  | Integer of int                   (* n *)
  | Variable of string               (* x *)
  | Add of a_exp * a_exp             (* a1 + a2 *)
  | Sub of a_exp * a_exp             (* a1 - a2 *)
  | Mul of a_exp * a_exp             (* a1 * a2 *)
  | Div of a_exp * a_exp             (* a1 / a2 *)
  | Mod of a_exp * a_exp             (* a1 % a2 *)
  | NotInt of a_exp                  (* -a1 *)

(* Boolean expressions; <=, >=, >=, == have been included *)
type b_exp =
  | Boolean of bool                    (* v *)
  | And of b_exp * b_exp               (* b1 and b2 *)
  | Or of b_exp * b_exp                (* b1 or b2 *)
  | NotBool of b_exp                   (* not b *)
  | LessThan of a_exp * a_exp          (* a1 < a2 *)
  | LessThanEqual of a_exp * a_exp     (* a1 <= a2 *)
  | GreaterThan of a_exp * a_exp       (* a1 > a2 *)
  | GreaterThanEqual of a_exp * a_exp  (* a1 >= a2 *)
  | Equal of a_exp * a_exp             (* a1 == a2 *)

(* Commands *)
type cmd =
  | Skip                            (* skip *)
  | Assign of string * a_exp        (* x := a *)
  | Seq of cmd * cmd                (* c1; c2 *)
  | If of b_exp * cmd * cmd         (* if b then c1 else c2 *)
  | While of b_exp * cmd            (* while b do c *)

(* Program *)
type program = 
  | Program of string * string * cmd  (* def main with input x output y as c *)

(* Memory:
Defining the environment ( = the memory) as a Map *)
module StringMap = Map.Make(String)
type memory = int StringMap.t

(* Lookup function *)
let lookup (var : string) (mem : memory) : int =
  try StringMap.find var mem
  with Not_found -> failwith ("Variable not found: " ^ var)

(* Update function *)
let update (var : string) (value : int) (mem : memory) : memory =
  StringMap.add var value mem

(* Evaluating arithmetic expressions *)
let rec eval_a_exp (exp : a_exp) (mem : memory) : int =
  match exp with
  | Integer n -> n
  | Variable var -> (
      try StringMap.find var mem
      with Not_found -> failwith ("Variable not found: " ^ var)
    )
  | Add (exp1, exp2) -> eval_a_exp exp1 mem + eval_a_exp exp2 mem
  | Sub (exp1, exp2) -> eval_a_exp exp1 mem - eval_a_exp exp2 mem
  | Mul (exp1, exp2) -> eval_a_exp exp1 mem * eval_a_exp exp2 mem
  | Div (exp1, exp2) ->
      let value = eval_a_exp exp2 mem in
      if value = 0 then failwith "Division by zero" else eval_a_exp exp1 mem / value
  | Mod (exp1, exp2) ->
      let value = eval_a_exp exp2 mem in
      if value = 0 then failwith "Modulo by zero" else eval_a_exp exp1 mem mod value
  | NotInt exp1 -> - (eval_a_exp exp1 mem)  

  (* Evaluating a boolean expression *)
  let rec eval_b_exp (bool : b_exp) (mem : memory) : bool =
  match bool with
  | Boolean bool -> bool 
  | And (bool1, bool2) -> (eval_b_exp bool1 mem) && (eval_b_exp bool2 mem)
  | Or (bool1, bool2) -> (eval_b_exp bool1 mem ) || (eval_b_exp bool2 mem) 
  | NotBool bool1 -> not (eval_b_exp bool1 mem)
  | LessThan (exp1, exp2) -> (eval_a_exp exp1 mem) < (eval_a_exp exp2 mem) 
  | LessThanEqual (exp1, exp2) -> (eval_a_exp exp1 mem) <= (eval_a_exp exp2 mem) 
  | GreaterThan (exp1, exp2) -> (eval_a_exp exp1 mem) > (eval_a_exp exp2 mem) 
  | GreaterThanEqual (exp1, exp2) -> (eval_a_exp exp1 mem) >= (eval_a_exp exp2 mem) 
  | Equal (exp1, exp2) -> (eval_a_exp exp1 mem) = (eval_a_exp exp2 mem) 
  
(* Evaluating a command *)
let rec eval_cmd (cmd : cmd) (mem : memory) : memory =
  match cmd with
  | Skip -> mem
  | Assign (var, exp) ->
      let value = eval_a_exp exp mem in
      StringMap.add var value mem
  | Seq (cmd1, cmd2) ->
      let new_mem = eval_cmd cmd1 mem in
      eval_cmd cmd2 new_mem
  | If (bool, then_cmd, else_cmd) ->
      if eval_b_exp bool mem
      then eval_cmd then_cmd mem  (* if bool is T, then branch *)
      else eval_cmd else_cmd mem  (* if bool is F, else branch *)
  | While (bool, cmd1) ->
      let rec loop mem = (* loop function definition *)
        if eval_b_exp bool mem 
        then loop (eval_cmd cmd1 mem) (* if bool is T, then loop *)
      else mem (* if bool is F, exit loop *)
      in
      loop mem (* first call of loop *)

(* Evaluating a program *)
let eval_program (prog : program) (i : int) : int =
  match prog with
  | Program (input, output, body) ->
      let mem = StringMap.add input i StringMap.empty in
      let new_mem = eval_cmd body mem in
      try StringMap.find output new_mem
      with Not_found -> failwith ("Output variable not found: " ^ output)

