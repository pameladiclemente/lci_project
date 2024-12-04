(* Project fragment 3: Complete the definition of the type system of MiniTyFun
extending the syntax of MiniFun with type annotations; produce an OCaml module for MiniTyFun, with an OCaml
type for the abstract syntax tree, and a type check function
that given a MiniTyFun term returns Someτ if τ is its type or None if it cannot be typed. *)

(* MiniTyFun syntax *)
module MiniTyFun = struct

  (* Types *)
  type ty =
    | IntType
    | BoolType
    | FunType of ty * ty

  (* Operators *)
  type op =
    | Add
    | Sub
    | Mul
    | LessThan
    | And
    | Not

  (* Terms with type annotations *)
  type term =
    | Int of int
    | Bool of bool
    | Var of string
    | Fun of string * ty * term
    | App of term * term
    | BinOp of op * term * term
    | If of term * term * term
    | Let of string * term * term
    | LetFun of string * string * ty * ty * term * term

  (* Defining the environment ( = the memory) as a Map *)
  (* Typing context: mapping variables to their types *)
  module StringMap = Map.Make(String)
  type memory = ty StringMap.t

  (* Lookup a variable's type in the context *)
  let lookup_type (x : string) (ctx : memory) : ty =
    try StringMap.find x ctx
    with Not_found -> failwith ("Variable '" ^ x ^ "' is not defined")

  (* Evaluating types *)
  let rec type_check (ctx : memory) (t : term) : ty =
    match t with
    | Int _ -> IntType
    | Bool _ -> BoolType
    | Var x -> lookup_type x ctx

    | BinOp (op, t1, t2) ->
        (match op, type_check ctx t1, type_check ctx t2 with
         | (Add | Sub | Mul), IntType, IntType -> IntType
         | LessThan, IntType, IntType -> BoolType
         | And, BoolType, BoolType -> BoolType
         | Not, BoolType, _ -> BoolType
         | _ -> failwith "Type error: invalid operands for binary operation")

    | If (t1, t2, t3) ->
        let cond_ty = type_check ctx t1 in
        let then_ty = type_check ctx t2 in
        let else_ty = type_check ctx t3 in
        if cond_ty = BoolType then
          if then_ty = else_ty then then_ty
          else failwith "Type error: branches of 'if' must have the same type"
        else failwith "Type error: 'if' condition must be of type bool"

    | Fun (x, ty_x, body) ->
        let ctx' = StringMap.add x ty_x ctx in
        let ty_body = type_check ctx' body in
        FunType (ty_x, ty_body)

    | App (t1, t2) ->
        (match type_check ctx t1 with
         | FunType (ty_param, ty_ret) ->
             let ty_arg = type_check ctx t2 in
             if ty_param = ty_arg then ty_ret
             else failwith "Type error: function argument type mismatch"
         | _ -> failwith "Type error: expected a function in application")
    | Let (x, t1, t2) ->
        let ty1 = type_check ctx t1 in
        let ctx' = StringMap.add x ty1 ctx in
        type_check ctx' t2
    | LetFun (f, x, ty_x, ty_ret, t1, t2) ->
        let ctx' = StringMap.add f (FunType (ty_x, ty_ret)) (StringMap.add x ty_x ctx) in
        let ty_body = type_check ctx' t1 in
        if ty_body = ty_ret then
          type_check (StringMap.add f (FunType (ty_x, ty_ret)) ctx) t2
        else failwith "Type error: recursive function return type mismatch"

end
