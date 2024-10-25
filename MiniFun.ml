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

(* Value type for MiniFun *)
type mem_value =
  | MemInt of int
  | MemBool of bool
  | Closure of string * term * memory
  | RecClosure of string * string * term * memory

(* Environment: a map from variable names to values *)
and memory = (string * mem_value) list

(* Lookup a variable in the environment *)
let rec lookup (x : string) (m : memory) : mem_value =
  match m with
  | [] -> failwith ("Variable not found: " ^ x)
  | (y, v) :: m2 -> if x = y then v else lookup x m2

(* Update the environment with a new variable binding *)
let update (x : string) (v : mem_value) (m : memory) : memory =
  (x, v) :: m



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
      | Closure (x, body, m) ->
          let new_env = update x input m in
          eval_term body new_env
      | RecClosure (f, x, body, m) ->
          let new_env = update x input (update f closure m) in
          eval_term body new_env
      | _ -> failwith "Pattern matching failure: expected a function")
    | Add (t1, t2) ->
      (match eval_term t1 m, eval_term t2 m with
      | MemInt t1, MemInt t2 -> MemInt (t1 + t2)
      | _ -> failwith "Pattern matching failure: unexpected type in addition")
    | Sub (t1, t2) ->
      (match eval_term t1 m, eval_term t2 m with
      | MemInt t1, MemInt t2 -> MemInt (t1 - t2)        
      | _ -> failwith "Pattern matching failure: unexpected type in addition")
    | Mul (t1, t2) ->
      (match eval_term t1 m, eval_term t2 m with
      | MemInt t1, MemInt t2 -> MemInt (t1 * t2)
      | _ -> failwith "Pattern matching failure: unexpected type in addition")
    | LessThan (t1, t2) ->
        (match eval_term t1 m, eval_term t2 m with
        | MemInt v1, MemInt v2 -> MemBool (v1 < v2)
        | _ -> failwith "Pattern matching failure: unexpected type in comparison")
    | And (t1, t2) ->
        (match eval_term t1 m, eval_term t2 m with
        | MemBool b1, MemBool b2 -> MemBool (b1 && b2)
        | _ -> failwith "Pattern matching failure: unexpected type in logical and")
    | Not t1 ->
        (match eval_term t1 m with
        | MemBool b1 -> MemBool (not b1)
        | _ -> failwith "Pattern matching failure: unexpected type in logical not")   
    | If (t1, t2, t3) ->
      let cond = eval_term t1 m in
      (match cond with
      |  MemBool true -> eval_term t2 m
      |  MemBool false -> eval_term t3 m
      |  _ -> failwith "Pattern matching failure: expected boolean in if condition")
    | Let (x, t1, t2) ->
      let v1 = eval_term t1 m in
      let m2 = update x v1 m in
      eval_term t2 m2
    | LetFun (f, x, t1, t2) ->
      let rec_closure = RecClosure (f, x, t1, m) in
      let m2 = update f rec_closure m in
      eval_term t2 m2
    | _ -> failwith "Invalid term in eval_term"
  