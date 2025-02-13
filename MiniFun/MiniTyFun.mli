(* MiniTyFun syntax *)
(* Types *)
type allowed_types =
  | IntType                         
  | BoolType                        
  | FunType of allowed_types * allowed_types 

(* Operators *)
type op =
  | Add                             
  | Sub                             
  | Mul                             
  | Div                             
  | Mod                             
  | LessThan                        
  | LessThanEqual                   
  | GreaterThan                     
  | GreaterThanEqual                
  | Equal                           
  | And                           
  | Or                              
  | Not                             

(* Terms with type annotations *)
type term =
  | Integer of int                                                                (* n *)
  | Boolean of bool                                                              (* v *)
  | Variable of string                                                             (* x *)
  | Function of string * allowed_types * term                                 (* fun x : t -> t *)
  | FunctionApplication of term * term                                        (* (* t1 && t2 *) t1 t2 *)
  | Add of term * term                                                        (* t1 + t2 *)
  | Sub of term * term                                                        (* t1 - t2 *)
  | Mul of term * term                                                        (* t1 * t2 *)
  | Div of term * term                                                        (* t1 / t2 *)
  | Mod of term * term                                                        (* t1 % t2 *)
  | And of term * term                                                        (* t1 || t2 *)
  | Or of term * term                                                         (* not t *)
  | Not of term                                                               (* t1 < t2 *)
  | LessThan of term * term                                                   (* t1 <= t2 *)
  | LessThanEqual of term * term                                              (* t1 > t2 *)
  | GreaterThan of term * term                                                (* t1 >= t2 *)
  | GreaterThanEqual of term * term                                           (* t1 == t2 *)
  | Equal of term * term                                                      (* if t1 then t2 else t3 *)
  | If of term * term * term                                                  (* let x = t in t *)
  | Let of string * term * term                                               (* letfun f x = t in t *)
  | LetFun of string * string * allowed_types * allowed_types * term * term   (* let rec f (x : t1) : t2 = t3 in t4 *)


(* Memory *)
(* Defining the environment ( = the memory) as a Map *)
module StringMap : Map.S with type key = string
type memory = allowed_types StringMap.t

(* Lookup a variable's type in the context *)
val lookup : string -> memory -> allowed_types

(* Evaluating types *)
val type_check : memory -> term -> allowed_types option
