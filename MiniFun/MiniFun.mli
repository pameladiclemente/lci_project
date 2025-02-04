(* MiniFun syntax *)
type term =
  | Integer of int                          (* n *)
  | Boolean of bool                         (* v *)
  | Variable of string                      (* x *)
  | Function of string * term               (* fun x => t *)
  | FunctionApplication of term * term      (* t1 t2 *)
  | Add of term * term                      (* t1 + t2 *)
  | Sub of term * term                      (* t1 - t2 *)
  | Mul of term * term                      (* t1 * t2 *)
  | Div of term * term                      (* t1 / t2 *)
  | Mod of term * term                      (* t1 % t2 *)
  | And of term * term                      (* t1 && t2 *)
  | Or of term * term                       (* t1 || t2 *)
  | Not of term                             (* not t *)
  | LessThan of term * term                 (* t1 < t2 *)
  | LessThanEqual of term * term            (* t1 <= t2 *)
  | GreaterThan of term * term              (* t1 > t2 *)
  | GreaterThanEqual of term * term         (* t1 >= t2 *)
  | Equal of term * term                    (* t1 == t2 *)
  | If of term * term * term                (* if t1 then t2 else t3 *)
  | Let of string * term * term             (* let x = t in t *)
  | LetFun of string * string * term * term (* letfun f x = t in t *)


(* Memory *)
(* Defining the environment ( = the memory) as a Map *)
module StringMap : Map.S with type key = string
type memoryAllowedValues =
  | MemInteger of int                             (* int *)
  | MemBoolean of bool                            (* bool *)
  | Closure of string * term * memory             (* fun x => t || let y = fun x => t in ⋅ *)
  | RecClosure of string * string * term * memory (* letfun f x = t in ⋅ *)

and memory = memoryAllowedValues StringMap.t

(* Lookup function *)
val lookup : string -> memory -> memoryAllowedValues

(* Update function *)
val update : string -> memoryAllowedValues -> memory -> memory

(* Evaluating terms *)
val eval_term : term -> memory -> memoryAllowedValues