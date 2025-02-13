(* Project fragment 3: Complete the definition of the type system of MiniTyFun
extending the syntax of MiniFun with type annotations; produce an OCaml module for MiniTyFun, with an OCaml
type for the abstract syntax tree, and a type check function
that given a MiniTyFun term returns Someτ if τ is its type or None if it cannot be typed. *)

(* MiniTyFun syntax *)
module MiniTyFun = struct

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
    | Integer of int                                                          (* n *)
    | Boolean of bool                                                         (* v *)
    | Variable of string                                                      (* x *)
    | Function of string * allowed_types * term                             (* t1 == t2 *)
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


  (* Defining the environment ( = the memory) as a Map *)
  (* Typing context: mapping variables to their types *)
  module StringMap = Map.Make(String)
  type memory = allowed_types StringMap.t

  (* Lookup a variable's type in the context *)
  let lookup (x : string) (m : memory) : allowed_types =
    try StringMap.find x m
    with Not_found -> failwith ("Variable not found: " ^ x)

  (* Evaluating types *)
 let rec type_check (m : memory) (t : term) : allowed_types option =
  match t with
  | Integer _ -> Some IntType
  | Boolean _ -> Some BoolType
  | Variable x -> (try Some (lookup x m) with Failure _ -> None)
  | Add (t1, t2)
  | Sub (t1, t2)
  | Mul (t1, t2)
  | Div (t1, t2)
  | Mod (t1, t2) ->
      (match (type_check m t1, type_check m t2) with
       | Some IntType, Some IntType -> Some IntType
       | _ -> None)
  | LessThan (t1, t2)
  | LessThanEqual (t1, t2)
  | GreaterThan (t1, t2)
  | GreaterThanEqual (t1, t2)
  | Equal (t1, t2) ->
      (match (type_check m t1, type_check m t2) with
       | Some IntType, Some IntType -> Some BoolType
       | _ -> None)
  | And (t1, t2)
  | Or (t1, t2) ->
      (match (type_check m t1, type_check m t2) with
       | Some BoolType, Some BoolType -> Some BoolType
       | _ -> None)
  | Not t ->
      (match type_check m t with
       | Some BoolType -> Some BoolType
       | _ -> None)
  | If (t1, t2, t3) ->
      (match type_check m t1, type_check m t2, type_check m t3 with
       | Some BoolType, Some type_t2, Some type_t3 when type_t2 = type_t3 -> Some type_t2
       | _ -> None)
  | Function (x, type_x, body) ->
      (match type_check (StringMap.add x type_x m) body with
       | Some type_body -> Some (FunType (type_x, type_body))
       | None -> None)
  | FunctionApplication (t1, t2) ->
      (match type_check m t1, type_check m t2 with
       | Some (FunType (type_param, type_return)), Some type_arg when type_param = type_arg -> Some type_return
       | _ -> None)
  | Let (x, t1, t2) ->
      (match type_check m t1 with
       | Some type_t1 -> type_check (StringMap.add x type_t1 m) t2
       | None -> None)
  | LetFun (f, x, type_x, type_return, t1, t2) ->
      let mem' = StringMap.add f (FunType (type_x, type_return)) (StringMap.add x type_x m) in
      (match type_check mem' t1 with
       | Some type_body when type_body = type_return -> type_check (StringMap.add f (FunType (type_x, type_return)) m) t2
       | _ -> None)
  end