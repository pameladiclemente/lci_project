(* Project fragment 2: Create a module for MiniFun that exposes
the type of the abstract syntax tree and an evaluation function. The
evaluation function can both fail and diverge, in agreement with the
semantics of MiniFun. *)

(* MiniFun syntax; division, modulo, <=, >=, >, == have been included *)
type term =
  | Integer of int                              (* n *)
  | Boolean of bool                             (* v *)
  | Variable of string                          (* x *)
  | Function of string * term                   (* fun x => t *)
  | FunctionApplication of string * term        (* f t2 *)
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
  let lookup (x : string) (m : memory) : memoryAllowedValues =
    try StringMap.find x m
    with Not_found -> failwith ("Variable not found: " ^ x)
  
  (* Update function *)
  let update (x : string) (v : memoryAllowedValues) (m : memory) : memory =
    StringMap.add x v m

  (* Evaluating terms *)
  let rec eval_term (t : term) (m : memory) : memoryAllowedValues =
    match t with
    | Integer n -> MemInteger n
    | Boolean b -> MemBoolean b
    | Variable x -> lookup x m
    | Function (f, t1) -> Closure (f, t1, m)
    | FunctionApplication (f, t1) ->
        let closure = lookup f m in
        let input_value = eval_term t1 m in
        (match closure with
          | Closure (f, t2, m1) -> 
              let m2 = update f input_value m1 in
              eval_term t2 m2
          | RecClosure (f, input, t2, m1) -> 
            let input_value = eval_term t1 m in (* Valutiamo l'argomento passato *)
            let m2 = update f closure m1 in (* Aggiungiamo la funzione stessa all'ambiente *)
            let m3 = update input input_value m2 in (* Aggiungiamo il parametro della funzione *)
            eval_term t2 m3
     | _ -> failwith "Error: expected a function but got a non-function value")
    | Add (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> MemInteger (n1 + n2)
         | _ -> failwith "Type error: addition requires two integers")
    | Sub (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> MemInteger (n1 - n2)
         | _ -> failwith "Type error: subtraction requires two integers")
    | Mul (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> MemInteger (n1 * n2)
         | _ -> failwith "Type error: multiplication requires two integers")
    | Div (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> 
          if n2 = 0 then failwith "Error: Division by zero"  
          else MemInteger (n1 / n2)
         | _ -> failwith "Type error: division requires two integers")
    | Mod (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInteger n1, MemInteger n2 -> 
            if n2 = 0 then failwith "Error: Modulo by zero"  
            else MemInteger (n1 mod n2)
         | _ -> failwith "Type error: modulo requires two integers")
    | And (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemBoolean b1, MemBoolean b2 -> MemBoolean (b1 && b2)
         | _ -> failwith "Type error: logical 'and' requires two booleans")
    | Or (t1, t2) ->
      let v1 = eval_term t1 m in
      let v2 = eval_term t2 m in
      (match v1, v2 with
      | MemBoolean b1, MemBoolean b2 -> MemBoolean (b1 || b2)
      | _ -> failwith "Type error: logical 'or' requires two booleans")
    | Not (t1) ->
        let v1 = eval_term t1 m in
        (match v1 with
        | MemBoolean b -> MemBoolean (not b)
        | MemInteger i -> MemInteger (-i)
        | _ -> failwith "Type error: logical 'not' requires a boolean or an integer")
    | LessThan (t1, t2) ->
      let v1 = eval_term t1 m in
      let v2 = eval_term t2 m in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 < n2)
       | _ -> failwith "Type error: comparison < requires two integers")
    | LessThanEqual (t1, t2) ->
      let v1 = eval_term t1 m in
      let v2 = eval_term t2 m in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 <= n2)
       | _ -> failwith "Type error: comparison <= requires two integers")
    | GreaterThan (t1, t2) ->
      let v1 = eval_term t1 m in
      let v2 = eval_term t2 m in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 > n2)
       | _ -> failwith "Type error: comparison > requires two integers")
    | GreaterThanEqual (t1, t2) ->
      let v1 = eval_term t1 m in
      let v2 = eval_term t2 m in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 >= n2)
       | _ -> failwith "Type error: comparison >= requires two integers")
    | Equal (t1, t2) ->
      let v1 = eval_term t1 m in
      let v2 = eval_term t2 m in
      (match v1, v2 with
       | MemInteger n1, MemInteger n2 -> MemBoolean (n1 == n2)
       | _ -> failwith "Type error: comparison == requires two integers")
    | If (t1, t2, t3) ->
        let b = eval_term t1 m in
        (match b with
         | MemBoolean true -> eval_term t2 m
         | MemBoolean false -> eval_term t3 m
         | _ -> failwith "Type error: 'if' condition must be a boolean")
    | Let (f, t1, t2) -> (*2 consecutive assignment to same variable x *)
        let v1 = eval_term t1 m in
        let m2 = update f v1 m in
        eval_term t2 m2
    | LetFun (f, x, t1, t2) ->  
        let rec_closure = RecClosure (f, x, t1, m) in  (* Wrappiamo x in Variable *)
        let m2 = update f rec_closure m in
        eval_term t2 m2


    