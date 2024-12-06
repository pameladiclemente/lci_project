(* Project fragment 1: Create a module for MiniImp that exposes the type of the abstract syntax tree and an evaluation function. *)
(* MiniImp syntax *)
(* Arithmetic expressions; division and modulo have been included *)
type a_exp =
  | Integer of int                 (* n *)
  | Variable of string             (* x *)
  | Add of a_exp * a_exp             (* a + a *)
  | Sub of a_exp * a_exp             (* a - a *)
  | Mul of a_exp * a_exp             (* a * a *)
  | Div of a_exp * a_exp             (* a / a *)
  | Mod of a_exp * a_exp             (* a % a *)

(* Boolean expressions; <=, >=, >, == have been included *)
type b_exp =
  | Boolean of bool                (* v *)
  | And of b_exp * b_exp             (* b and b *)
  | Or of b_exp * b_exp              (* b or b *)
  | Not of b_exp                    (* not b *)
  | LessThan of a_exp * a_exp        (* a < a *)
  | LessThanEqual of a_exp * a_exp   (* a <= a *)
  | GreaterThan of a_exp * a_exp     (* a > a *)
  | GreaterThanEqual of a_exp * a_exp (* a >= a *)
  | Equal of a_exp * a_exp (* a == a *)

(* Commands *)
type cmd =
  | Skip                           (* skip *)
  | Assign of string * a_exp        (* x := a *)
  | Seq of cmd * cmd               (* c; c *)
  | If of b_exp * cmd * cmd         (* if b then c else c *)
  | While of b_exp * cmd            (* while b do c *)

(* Program *)
type program = 
  | Program of string * string * cmd  (* def main with input x output y as c *)


(* Memory *)
(* Defining the environment ( = the memory) as a Map *)
module StringMap = Map.Make(String)
type memory = int StringMap.t

(* Lookup function with error handling *)
let lookup (x : string) (m : memory) : int =
  try StringMap.find x m
  with Not_found -> failwith ("Variable not found in the memory: " ^ x)

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
  let rec eval_b_exp (b : b_exp) (m : memory) : bool =
  match b with
  | Boolean b -> b
  | And (b1, b2) -> eval_b_exp b1 m && eval_b_exp b2 m
  | Or (b1, b2) -> eval_b_exp b1 m || eval_b_exp b2 m
  | Not b1 -> not (eval_b_exp b1 m)
  | LessThan (a1, a2) -> eval_a_exp a1 m < eval_a_exp a2 m
  | LessThanEqual (a1, a2) -> eval_a_exp a1 m <= eval_a_exp a2 m
  | GreaterThan (a1, a2) -> eval_a_exp a1 m > eval_a_exp a2 m
  | GreaterThanEqual (a1, a2) -> eval_a_exp a1 m >= eval_a_exp a2 m
  | Equal (a1, a2) -> eval_a_exp a1 m == eval_a_exp a2 m
  
(* Evaluating a command *)
let rec eval_cmd (c : cmd) (m : memory) : memory =
  match c with
  | Skip -> m
  | Assign (x, a) ->
      let v = eval_a_exp a m in
      StringMap.add x v m
  | Seq (c1, c2) ->
      let m' = eval_cmd c1 m in
      eval_cmd c2 m'
  | If (b, c1, c2) ->
      if eval_b_exp b m then eval_cmd c1 m else eval_cmd c2 m
  | While (b, c1) ->
      let rec loop m' =
        if eval_b_exp b m' then loop (eval_cmd c1 m') else m'
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
