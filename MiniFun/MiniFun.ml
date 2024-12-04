(* Project fragment 2: Create a module for MiniFun that exposes
the type of the abstract syntax tree and an evaluation function. The
evaluation function can both fail and diverge, in agreement with the
semantics of MiniFun. *)
(* MiniFun syntax *)
type term =
  | Integer of int
  | Boolean of bool
  | Variable of string
  | Function of string * term
  | App of term * term
  | Add of term * term
  | Sub of term * term
  | Mul of term * term
  | LessThan of term * term
  | And of term * term
  | Not of term
  | If of term * term * term
  | Let of string * term * term
  | LetFun of string * string * term * term


  (* Defining the environment ( = the memory) as a Map *)
  module StringMap = Map.Make(String)
  type mem_value =
    | MemInt of int
    | MemBool of bool
    | Closure of string * term * memory
    | RecClosure of string * string * term * memory
  and memory = mem_value StringMap.t
  
  (* Lookup a variable in the environment *)
  let lookup (x : string) (m : memory) : mem_value =
    try StringMap.find x m
    with Not_found -> failwith ("Variable not found: " ^ x)
  
  (* Update the environment with a new variable binding *)
  let update (x : string) (v : mem_value) (m : memory) : memory =
    StringMap.add x v m

(* Evaluating terms *)
  let rec eval_term (t : term) (m : memory) : mem_value =
    match t with
    | Integer n -> MemInt n
    | Boolean b -> MemBool b
    | Variable x -> lookup x m
    | Function (x, body) -> Closure (x, body, m)
    | App (t1, t2) ->
        let closure = eval_term t1 m in
        let input = eval_term t2 m in
        (match closure with
         | Closure (x, body, closure_env) ->
             let new_env = update x input closure_env in
             eval_term body new_env
         | RecClosure (f, x, body, closure_env) ->
             let new_env = update x input (update f closure closure_env) in
             eval_term body new_env
         | _ -> failwith "Application error: expected a function but got a non-function value")
    | Add (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInt n1, MemInt n2 -> MemInt (n1 + n2)
         | _ -> failwith "Type error: addition requires two integers")
    | Sub (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInt n1, MemInt n2 -> MemInt (n1 - n2)
         | _ -> failwith "Type error: subtraction requires two integers")
    | Mul (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInt n1, MemInt n2 -> MemInt (n1 * n2)
         | _ -> failwith "Type error: multiplication requires two integers")
    | LessThan (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemInt n1, MemInt n2 -> MemBool (n1 < n2)
         | _ -> failwith "Type error: comparison requires two integers")
    | And (t1, t2) ->
        let v1 = eval_term t1 m in
        let v2 = eval_term t2 m in
        (match v1, v2 with
         | MemBool b1, MemBool b2 -> MemBool (b1 && b2)
         | _ -> failwith "Type error: logical 'and' requires two booleans")
    | Not t1 ->
        let v = eval_term t1 m in
        (match v with
         | MemBool b -> MemBool (not b)
         | _ -> failwith "Type error: logical 'not' requires a boolean")
    | If (t1, t2, t3) ->
        let cond = eval_term t1 m in
        (match cond with
         | MemBool true -> eval_term t2 m
         | MemBool false -> eval_term t3 m
         | _ -> failwith "Type error: 'if' condition must be a boolean")
    | Let (x, t1, t2) ->
        let v1 = eval_term t1 m in
        let m2 = update x v1 m in
        eval_term t2 m2
    | LetFun (f, x, t1, t2) ->
        let rec_closure = RecClosure (f, x, t1, m) in
        let m2 = update f rec_closure m in
        eval_term t2 m2
    