(* MiniImp syntax *)
type exp =
  | Integer of int               
  | Boolean of bool              
  | Variable of string           
  | Add of exp * exp             
  | Sub of exp * exp             
  | Mul of exp * exp             
  | LessThan of exp * exp        
  | And of exp * exp             
  | Not of exp                   
  | Skip                         
  | Assign of string * exp        
  | Seq of exp * exp             
  | If of exp * exp * exp        
  | While of exp * exp           
  | Program of string * string * exp  


  (* Defining the environment ( = the memory) as a Map *)
module StringMap = Map.Make(String)
type mem_value =
| MemInt of int
| MemBool of bool

type memory = mem_value StringMap.t

let lookup (x : string) (m : memory) : mem_value =
  try StringMap.find x m
  with Not_found -> failwith ("Variable is not in the memory: " ^ x)

let update (x : string) (v : mem_value) (m : memory) : memory =
  StringMap.add x v m

(* Evaluating expressions *)
let rec eval_expr (e : exp) (m : memory) : mem_value =
  match e with
  | Integer n -> MemInt n
  | Boolean b -> MemBool b
  | Variable x -> lookup x m
  | Add (e1, e2) ->
      (match eval_expr e1 m, eval_expr e2 m with
      | MemInt v1, MemInt v2 -> MemInt (v1 + v2)
      | _ -> failwith "Pattern matching failure: unexpected type in addition")
  | Sub (e1, e2) ->
      (match eval_expr e1 m, eval_expr e2 m with
      | MemInt v1, MemInt v2 -> MemInt (v1 - v2)
      | _ -> failwith "Pattern matching failure: unexpected type in subtraction")
  | Mul (e1, e2) ->
      (match eval_expr e1 m, eval_expr e2 m with
      | MemInt v1, MemInt v2 -> MemInt (v1 * v2)
      | _ -> failwith "Pattern matching failure: unexpected type in multiplication")
  | LessThan (e1, e2) ->
      (match eval_expr e1 m, eval_expr e2 m with
      | MemInt v1, MemInt v2 -> MemBool (v1 < v2)
      | _ -> failwith "Pattern matching failure: unexpected type in comparison")
  | And (e1, e2) ->
      (match eval_expr e1 m, eval_expr e2 m with
      | MemBool b1, MemBool b2 -> MemBool (b1 && b2)
      | _ -> failwith "Pattern matching failure: unexpected type in logical and")
  | Not e1 ->
      (match eval_expr e1 m with
      | MemBool b1 -> MemBool (not b1)
      | _ -> failwith "Pattern matching failure: unexpected type in logical not")
  | _ -> failwith "Invalid expression in eval_expr"


(* Evaluating commands *)
let rec eval_cmd (c : exp) (m1 : memory) : memory =
  match c with
  | Skip -> m1  (* skip does nothing *)
  | Assign (x, e) -> 
      let v = eval_expr e m1 in
      (match v with
        | MemInt _ | MemBool _ -> update x v m1  (* Update the variable in the memory *)
        | _ -> failwith ("Pattern matching failure: assigning an unsupported type to variable " ^ x))
  | Seq (c1, c2) -> 
      let m2 = eval_cmd c1 m1 in
      eval_cmd c2 m2  (* Execute the second command after the first *)
  | If (b, c1, c2) -> 
    let v = eval_expr b m1 in
      (match eval_expr b m1 with
      | MemBool true -> eval_cmd c1 m1  (* Execute then-branch *)
      | MemBool false -> eval_cmd c2 m1  (* Execute else-branch *)
      | MemInt _ -> failwith "Pattern matching failure: expected boolean in 'if' condition, but got an integer"
      | _ -> failwith "Pattern matching failure in if condition")
  | While (b, c1) -> 
      let rec loop m2 =
        match eval_expr b m2 with
        | MemBool true -> loop (eval_cmd c1 m2)  (* Continue the loop *)
        | MemBool false -> m2  (* Exit the loop *)
        | MemInt _ -> failwith "Pattern matching failure: expected boolean in 'while' condition, but got an integer"
        | _ -> failwith "Type error in while condition"
      in
      loop m1
  | _ -> failwith "Invalid command in eval_cmd"


(* Evaluating and executing a full program *)
let eval_program (p : exp) (i : int) : int =
  match p with
  | Program (input, output, body) ->
      let m1 = StringMap.add input (MemInt i) StringMap.empty in
      let m2 = eval_cmd body m1 in
      (match lookup output m2 with
      | MemInt v -> v
      | _ -> failwith "Output variable is not an integer")
  | _ -> failwith "Not a valid program"

 