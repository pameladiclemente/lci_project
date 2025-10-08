(* Project fragment 2: Create a module for MiniFun that exposes
the type of the abstract syntax tree and an evaluation function. The
evaluation function can both fail and diverge, in agreement with the
semantics of MiniFun. *)

(* MiniFun syntax; not, division, modulo, <=, >=, >, = have been included *)
type term =
  | Integer of int                              (* n *)
  | Boolean of bool                             (* v *)
  | Variable of string                          (* x *)
  | Function of string * term                   (* fun x => t *)
  | FunctionApplication of term * term          (* t t *)
  | Add of term * term                          (* t1 + t2 *)
  | Sub of term * term                          (* t1 - t2 *)
  | Mul of term * term                          (* t1 * t2 *)
  | Div of term * term                          (* t1 / t2 *)
  | Mod of term * term                          (* t1 % t2 *)
  | And of term * term                          (* t1 && t2 *)
  | Or of term * term                           (* t1 || t2 *)
  | Not of term                                 (* not t *)
  | LessThan of term * term                     (* t1 < t2 *)
  | LessThanEqual of term * term                (* t1 <= t2 *)
  | GreaterThan of term * term                  (* t1 > t2 *)
  | GreaterThanEqual of term * term             (* t1 >= t2 *)
  | Equal of term * term                        (* t1 == t2 *)
  | If of term * term * term                    (* if t1 then t2 else t3 *)
  | Let of string * term * term                 (* let x = t in t *)
  | LetFun of string * string * term * term     (* letfun f x = t in t *)

  (* Defining the environment ( = the memory) as a Map *)
  module StringMap = Map.Make(String)
  type memoryAllowedValues =
    | MemInteger of int                               (* int *)        
    | MemBoolean of bool                              (* bool *)
    | Closure of string * term * memory               (* fun x => t || let y = fun x => t in ⋅ *)
    | RecClosure of string * string * term * memory   (* letfun f x = t in ⋅ *)
  and memory = memoryAllowedValues StringMap.t              
  
  (* Lookup function *)
  let lookup (var : string) (mem : memory) : memoryAllowedValues =
    try StringMap.find var mem
    with Not_found -> failwith ("Variable not found: " ^ var)
  
  (* Update function *)
  let update (var : string) (value : memoryAllowedValues) (mem : memory) : memory =
    StringMap.add var value mem

  (* Evaluating terms *)
  let rec eval_term (t : term) (mem : memory) : memoryAllowedValues =
    match t with
    | Integer n -> MemInteger n
    | Boolean bool -> MemBoolean bool
    | Variable var -> lookup var mem
    | Function (f, body) -> Closure (f, body, mem)
    | FunctionApplication (t1, t2) ->
      let closure = eval_term t1 mem in   
      let arg_val = eval_term t2 mem in
      (match closure with
      | Closure (var, body, mem) ->
          let new_mem = update var arg_val mem in
          eval_term body new_mem
      | RecClosure (f, var, body, mem) ->
          let new_mem = update f closure mem in
          let new_mem2 = update var arg_val new_mem in
          eval_term body new_mem2
      | _ -> failwith "Type error: expected function but got a non-function value")
    | Add (t1, t2) ->
        let v1 = eval_term t1 mem in
        let v2 = eval_term t2 mem in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> MemInteger (n1 + n2)
         | _ -> failwith "Type error: sum requires two integers")
    | Sub (t1, t2) ->
        let v1 = eval_term t1 mem in
        let v2 = eval_term t2 mem in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> MemInteger (n1 - n2)
         | _ -> failwith "Type error: subtraction requires two integers")
    | Mul (t1, t2) ->
        let v1 = eval_term t1 mem in
        let v2 = eval_term t2 mem in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> MemInteger (n1 * n2)
         | _ -> failwith "Type error: product requires two integers")
    | Div (t1, t2) ->
        let v1 = eval_term t1 mem in
        let v2 = eval_term t2 mem in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> 
          if n2 = 0 then failwith "Error: Division by zero"  
          else MemInteger (n1 / n2)
         | _ -> failwith "Type error: division requires two integers")
    | Mod (t1, t2) ->
        let v1 = eval_term t1 mem in
        let v2 = eval_term t2 mem in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> 
            if n2 = 0 then failwith "Error: Modulo by zero"  
            else MemInteger (n1 mod n2)
         | _ -> failwith "Type error: modulo requires two integers")
    | And (t1, t2) ->
        let v1 = eval_term t1 mem in
        let v2 = eval_term t2 mem in
        (match v1, v2 with
         | MemBoolean b1, MemBoolean b2 -> MemBoolean (b1 && b2)
         | _ -> failwith "Type error: logical 'and' requires two booleans")
    | Or (t1, t2) ->
      let v1 = eval_term t1 mem in
      let v2 = eval_term t2 mem in
      (match v1, v2 with
      | MemBoolean b1, MemBoolean b2 -> MemBoolean (b1 || b2)
      | _ -> failwith "Type error: logical 'or' requires two booleans")
    | Not (t1) ->
        let v1 = eval_term t1 mem in
        (match v1 with
        | MemBoolean b -> MemBoolean (not b)
        | MemInteger i -> MemInteger (-i)
        | _ -> failwith "Type error: logical 'not' requires a boolean or an integer")
    | LessThan (t1, t2) ->
      let v1 = eval_term t1 mem in
      let v2 = eval_term t2 mem in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 < n2)
       | _ -> failwith "Type error: comparison < requires two integers")
    | LessThanEqual (t1, t2) ->
      let v1 = eval_term t1 mem in
      let v2 = eval_term t2 mem in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 <= n2)
       | _ -> failwith "Type error: comparison <= requires two integers")
    | GreaterThan (t1, t2) ->
      let v1 = eval_term t1 mem in
      let v2 = eval_term t2 mem in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 > n2)
       | _ -> failwith "Type error: comparison > requires two integers")
    | GreaterThanEqual (t1, t2) ->
      let v1 = eval_term t1 mem in
      let v2 = eval_term t2 mem in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 >= n2)
       | _ -> failwith "Type error: comparison >= requires two integers")
    | Equal (t1, t2) ->
      let v1 = eval_term t1 mem in
      let v2 = eval_term t2 mem in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 == n2)
       | _ -> failwith "Type error: comparison == requires two integers")
    | If (bool, then_term, else_term) ->
        let b = eval_term bool mem in
        (match b with
         | MemBoolean true -> eval_term then_term mem
         | MemBoolean false -> eval_term else_term mem
         | _ -> failwith "Type error: 'if' condition must be a boolean")
    | Let (f, t1, t2) -> (* 2 consecutive assignment to same variable x *)
        let value = eval_term t1 mem in
        let new_mem = update f value mem in
        eval_term t2 new_mem
    | LetFun (f, var, body, t2) ->  
        let rec_closure = RecClosure (f, var, body, mem) in  (* Wrappiamo x in Variable *)
        let new_mem = update f rec_closure mem in
        eval_term t2 new_mem


    