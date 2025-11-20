(* Project fragment: 
Complete the definition of the type system of MiniTyFun extending the syntax of MiniFun with type annotations; 
produce an OCaml module for MiniTyFun, with an OCaml type for the abstract syntax tree, 
and a type check function that given a MiniTyFun term returns Some τ if τ is its type 
or None if it cannot be typed. *)

(* MiniTyFun syntax *)

(* Types *)
type allowed_types =
  | IntType 
  | BoolType 
  | FunType of allowed_types * allowed_types

(* Terms with type annotations *)
type term =
  | Integer of int                                                          (* n *)
  | Boolean of bool                                                         (* v *)
  | Variable of string                                                      (* x *)
  | Function of string * allowed_types * term                               (* t1 == t2 *)
  | FunctionApplication of term * term                                      (* let x = t in t *)
  | Add of term * term                                                      (* fun x : t -> t *)
  | Sub of term * term                                                      (* (* t1 && t2 *) t1 t2 *)
  | Mul of term * term                                                      (* t1 + t2 *)
  | Div of term * term                                                      (* t1 - t2 *)
  | Mod of term * term                                                      (* t1 * t2 *)
  | LessThan of term * term                                                 (* t1 / t2 *)
  | LessThanEqual of term * term                                            (* t1 % t2 *)
  | GreaterThan of term * term                                              (* t1 || t2 *)
  | GreaterThanEqual of term * term                                         (* not t *)
  | Equal of term * term                                                    (* t1 < t2 *)
  | And of term * term                                                      (* t1 <= t2 *)
  | Or of term * term                                                       (* t1 > t2 *)
  | Not of term                                                             (* t1 >= t2 *)
  | If of term * term * term                                                (* if t1 then t2 else t3 *) 
  | Let of string * term * term                                             (* letfun f x = t in t *)
  | LetFun of string * string * allowed_types * allowed_types * term * term (* let rec f (x : t1) : t2 = t3 in t4 *)


(* Memory:
Defining such environment as a Map mapping variables to their types, not values *)
module StringMap = Map.Make(String)
type memory = allowed_types StringMap.t

(* Lookup a variable's type in the context *)
let lookup (var : string) (mem : memory) : allowed_types option =
  try Some (StringMap.find var mem)
  with Not_found -> None

(* Evaluating types *)
let rec check_type (mem : memory) (t : term) : allowed_types option =
  match t with
  | Integer n -> Some IntType
  | Boolean bool -> Some BoolType
  | Variable var -> lookup var mem
  | Add (t1, t2)
  | Sub (t1, t2) 
  | Mul (t1, t2) 
  | Div (t1, t2) 
  | Mod (t1, t2) ->
      (match (check_type mem t1, check_type mem t2) with
       | Some IntType, Some IntType -> Some IntType
       | _ -> None)
  | LessThan (t1, t2)
  | LessThanEqual (t1, t2)
  | GreaterThan (t1, t2)
  | GreaterThanEqual (t1, t2)
  | Equal (t1, t2) ->
      (match (check_type mem t1, check_type mem t2) with
       | Some IntType, Some IntType -> Some BoolType
       | _ -> None)
  | And (t1, t2)
  | Or (t1, t2) ->
      (match (check_type mem t1, check_type mem t2) with
       | Some BoolType, Some BoolType -> Some BoolType
       | _ -> None)
  | Not t ->
      (match check_type mem t with
       | Some BoolType -> Some BoolType
       | _ -> None)
  | If (t1, t2, t3) ->
      (match check_type mem t1, check_type mem t2, check_type mem t3 with
       | Some BoolType, Some type_t2, Some type_t3 when type_t2 = type_t3 -> Some type_t2
       | _ -> None)
  | Function (var, type_var, body) ->
      (match check_type (StringMap.add var type_var mem) body with
       | Some type_body -> Some (FunType (type_var, type_body))
       | None -> None)
  | FunctionApplication (f, var) ->
      (match check_type mem f, check_type mem var with
       | Some (FunType (type_param, type_return)), Some type_arg when type_param = type_arg -> Some type_return
       | _ -> None)
  | Let (var, value, body) ->
      (match check_type mem value with
       | Some type_t1 -> check_type (StringMap.add var type_t1 mem) body
       | None -> None)
  | LetFun (f, var, type_var, type_return, body, expr) ->
      let new_mem = StringMap.add f (FunType (type_var, type_return)) (StringMap.add var type_var mem) in
      (match check_type new_mem body with
       | Some type_body when type_body = type_return -> check_type (StringMap.add f (FunType (type_var, type_return)) mem) expr
       | _ -> None)
  