(* MiniTyFun syntax *)

(* Types *)
type allowed_types =
  | IntType
  | BoolType
  | FunType of allowed_types * allowed_types

(* Terms with type annotations *)
type term =
  | Integer of int (* n *)
  | Boolean of bool (* v *)
  | Variable of string (* x *)
  | Function of string * allowed_types * term (* t1 == t2 *)
  | FunctionApplication of term * term (* let x = t in t *)
  | Add of term * term (* fun x : t -> t *)
  | Sub of term * term (* (* t1 && t2 *) t1 t2 *)
  | Mul of term * term (* t1 + t2 *)
  | Div of term * term (* t1 - t2 *)
  | Mod of term * term (* t1 * t2 *)
  | LessThan of term * term (* t1 / t2 *)
  | LessThanEqual of term * term (* t1 % t2 *)
  | GreaterThan of term * term (* t1 || t2 *)
  | GreaterThanEqual of term * term (* not t *)
  | Equal of term * term (* t1 < t2 *)
  | And of term * term (* t1 <= t2 *)
  | Or of term * term (* t1 > t2 *)
  | Not of term (* t1 >= t2 *)
  | If of term * term * term (* if t1 then t2 else t3 *)
  | Let of string * term * term (* letfun f x = t in t *)
  | LetFun of
      string
      * string
      * allowed_types
      * allowed_types
      * term
      * term (* let rec f (x : t1) : t2 = t3 in t4 *)

(* Memory:
  Defining such environment as a Map mapping variables to their types, not values *)
module StringMap : Map.S with type key = string

type memory = allowed_types StringMap.t

(* Lookup a variable's type in the context *)
val lookup : string -> memory -> allowed_types option

(* Evaluating types *)
val check_type : memory -> term -> allowed_types option
