(* Arithmetic expressions; 
division, modulo and negative numbers have been included *)
type a_exp =
  | Integer of int (* n *)
  | Variable of string (* x *)
  | Add of a_exp * a_exp (* a1 + a2 *)
  | Sub of a_exp * a_exp (* a1 - a2 *)
  | Mul of a_exp * a_exp (* a1 * a2 *)
  | Div of a_exp * a_exp (* a1 / a2 *)
  | Mod of a_exp * a_exp (* a1 % a2 *)
  | NotInt of a_exp (* -a1 *)

(* Boolean expressions; 
<=, >=, >, ==, ! have been included *)
type b_exp =
  | Boolean of bool (* 0 for false, 1 for true *)
  | And of b_exp * b_exp (* b1 and b2 *)
  | Or of b_exp * b_exp (* b1 or b2 *)
  | NotBool of b_exp (* not b *)
  | LessThan of a_exp * a_exp (* a1 < a2 *)
  | LessThanEqual of a_exp * a_exp (* a1 <= a2 *)
  | GreaterThan of a_exp * a_exp (* a1 > a2 *)
  | GreaterThanEqual of a_exp * a_exp (* a1 >= a2 *)
  | Equal of a_exp * a_exp (* a1 == a2 *)

(* Commands *)
type cmd =
  | Skip (* skip *)
  | Assign of string * a_exp (* x := a *)
  | Seq of cmd * cmd (* c1; c2 *)
  | If of b_exp * cmd * cmd (* if b then c1 else c2 *)
  | While of b_exp * cmd (* while b do c *)

(* Program *)
type program =
  | Program of string * string * cmd (* def main with input x output y as c *)

(* Memory:
Defining such environment as a Map of strings to integers. *)
module StringMap : Map.S with type key = string

type memory = int StringMap.t

(* Lookup function *)
val lookup : string -> memory -> int

(* Update function *)
val update : string -> int -> memory -> memory

(* Evaluating a program *)
val eval_program : program -> int -> int
