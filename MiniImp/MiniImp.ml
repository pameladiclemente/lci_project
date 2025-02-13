(* Project fragment 1: 
Create a module for MiniImp that exposes the type of the abstract syntax tree and an evaluation function. *)

(* MiniImp syntax 
 Arithmetic expressions; division and modulo have been included *)
type a_exp =
  | Integer of int                   (* n *)
  | Variable of string               (* x *)
  | Add of a_exp * a_exp             (* a1 + a2 *)
  | Sub of a_exp * a_exp             (* a1 - a2 *)
  | Mul of a_exp * a_exp             (* a1 * a2 *)
  | Div of a_exp * a_exp             (* a1 / a2 *)
  | Mod of a_exp * a_exp             (* a1 % a2 *)

(* Boolean expressions; <=, >=, >, == have been included *)
type b_exp =
  | Boolean of int                    (* 0 for false, 1 for true *)
  | And of b_exp * b_exp               (* b1 and b2 *)
  | Or of b_exp * b_exp                (* b1 or b2 *)
  | Not of b_exp                       (* not b *)
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
let lookup (x : string) (m : memory) : int =
  try StringMap.find x m
  with Not_found -> failwith ("Variable not found: " ^ x)

(* Update function *)
let update (x : string) (v : int) (m : memory) : memory =
  StringMap.add x v m

(* Evaluating arithmetic expressions *)
let rec eval_a_exp (a : a_exp) (m : memory) : int =
  match a with
  | Integer n -> n
  | Variable x -> (
      try StringMap.find x m
      with Not_found -> failwith ("Variable not found: " ^ x)
    )
  | Add (a1, a2) -> eval_a_exp a1 m + eval_a_exp a2 m
  | Sub (a1, a2) -> eval_a_exp a1 m - eval_a_exp a2 m
  | Mul (a1, a2) -> eval_a_exp a1 m * eval_a_exp a2 m
  | Div (a1, a2) ->
      let v2 = eval_a_exp a2 m in
      if v2 = 0 then failwith "Division by zero" else eval_a_exp a1 m / v2
  | Mod (a1, a2) ->
      let v2 = eval_a_exp a2 m in
      if v2 = 0 then failwith "Modulo by zero" else eval_a_exp a1 m mod v2

  (* Evaluating a boolean expression *)
  let rec eval_b_exp (b : b_exp) (m : memory) : int =
  match b with
  | Boolean b -> if b <> 0 then 1 else 0 
  | And (b1, b2) -> if eval_b_exp b1 m = 1 && eval_b_exp b2 m = 1 then 1 else 0
  | Or (b1, b2) -> if eval_b_exp b1 m = 1 || eval_b_exp b2 m = 1 then 1 else 0
  | Not b1 -> if eval_b_exp b1 m = 1 then 0 else 1
  | LessThan (a1, a2) -> if eval_a_exp a1 m < eval_a_exp a2 m then 1 else 0
  | LessThanEqual (a1, a2) -> if eval_a_exp a1 m <= eval_a_exp a2 m then 1 else 0
  | GreaterThan (a1, a2) -> if eval_a_exp a1 m > eval_a_exp a2 m then 1 else 0
  | GreaterThanEqual (a1, a2) -> if eval_a_exp a1 m >= eval_a_exp a2 m then 1 else 0
  | Equal (a1, a2) -> if eval_a_exp a1 m = eval_a_exp a2 m then 1 else 0
  
(* Evaluating a command *)
let rec eval_cmd (c : cmd) (m : memory) : memory =
  match c with
  | Skip -> m
  | Assign (x, a) ->
      let v = eval_a_exp a m in
      StringMap.add x v m
  | Seq (c1, c2) ->
      let m2 = eval_cmd c1 m in
      eval_cmd c2 m2
  | If (b, c1, c2) ->
      if eval_b_exp b m = 1 then eval_cmd c1 m else eval_cmd c2 m  (* Confronto con 1 *)
  | While (b, c1) ->
      let rec loop m2 =
        if eval_b_exp b m2 = 1 then loop (eval_cmd c1 m2) else m2  (* Confronto con 1 *)
      in
      loop m

(* Evaluating a program *)
let eval_program (p : program) (i : int) : int =
  match p with
  | Program (input, output, body) ->
      let m1 = StringMap.add input i StringMap.empty in
      let m2 = eval_cmd body m1 in
      try StringMap.find output m2
      with Not_found -> failwith ("Output variable not found: " ^ output)

