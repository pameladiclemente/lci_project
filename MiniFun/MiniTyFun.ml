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
    | Int of int                                                              (* n *)
    | Bool of bool                                                            (* v *)
    | Var of string                                                           (* x *)
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
    | Fun of string * allowed_types * term                                    (* t1 == t2 *)
    | App of term * term                                                      (* if t1 then t2 else t3 *)
    | If of term * term * term                                                (* let x = t in t *)
    | Let of string * term * term                                             (* letfun f x = t in t *)
    | LetFun of string * string * allowed_types * allowed_types * term * term (* let rec f (x : t1) : t2 = t3 in t4 *)


  (* Defining the environment ( = the memory) as a Map *)
  (* Typing context: mapping variables to their types *)
  module StringMap = Map.Make(String)
  type memory = allowed_types StringMap.t

  (* Lookup a variable's type in the context *)
  let lookup_type (x : string) (ctx : memory) : allowed_types =
    try StringMap.find x ctx
    with Not_found -> failwith ("Variable '" ^ x ^ "' is not defined")

  (* Evaluating types *)
  let rec type_check (mem : memory) (t : term) : allowed_types =
    match t with
    | Int n -> IntType 
    | Bool b -> BoolType 
    | Var x -> lookup_type x mem
    | Add (t1, t2)
    | Sub (t1, t2)
    | Mul (t1, t2)
    | Div (t1, t2)
    | Mod (t1, t2) ->
        (match (type_check mem t1, type_check mem t2) with
          | (IntType, IntType) -> IntType 
          | _ -> failwith "Type error: arithmetic operations require both integers")
    | LessThan (t1, t2)
    | LessThanEqual (t1, t2)
    | GreaterThan (t1, t2)
    | GreaterThanEqual (t1, t2)
    | Equal (t1, t2) ->
        (match (type_check mem t1, type_check mem t2) with
          | (IntType, IntType) -> BoolType
          | _ -> failwith "Type error: comparison operations require both integers")
    | And (t1, t2)
    | Or (t1, t2) ->
        (match (type_check mem t1, type_check mem t2) with
          | (BoolType, BoolType) -> BoolType 
          | _ -> failwith "Type error: logical operations require both booleans")
    | Not t ->
        (match type_check mem t with
          | BoolType -> BoolType
          | _ -> failwith "Type error: 'not' requires a boolean")
    | If (t1, t2, t3) ->
        let cond_ty = type_check mem t1 in
        let then_ty = type_check mem t2 in
        let else_ty = type_check mem t3 in
        if cond_ty = BoolType then
          if then_ty = else_ty then then_ty
          else failwith "Type error: branches of 'if' must have the same type"
        else failwith "Type error: 'if' condition must be of type bool"
    | Fun (x, ty_x, body) ->
        let mem' = StringMap.add x ty_x mem in
        let ty_body = type_check mem' body in
        FunType (ty_x, ty_body)
    | App (t1, t2) ->
        (match type_check mem t1 with
         | FunType (ty_param, ty_ret) ->
             let ty_arg = type_check mem t2 in
             if ty_param = ty_arg then ty_ret
             else failwith "Type error: function argument type mismatch"
         | _ -> failwith "Type error: expected a function in application")
    | Let (x, t1, t2) ->
        let ty1 = type_check mem t1 in
        let mem' = StringMap.add x ty1 mem in
        type_check mem' t2
    | LetFun (f, x, ty_x, ty_ret, t1, t2) ->
        let mem' = StringMap.add f (FunType (ty_x, ty_ret)) (StringMap.add x ty_x mem) in
        let ty_body = type_check mem' t1 in
        if ty_body = ty_ret then
          type_check (StringMap.add f (FunType (ty_x, ty_ret)) mem) t2
        else failwith "Type error: recursive function return type mismatch"
end
