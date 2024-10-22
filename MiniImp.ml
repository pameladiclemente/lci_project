(* syntax *)
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

  (* The memory (also called an environment) will store the values of variables 
  during program execution. You'll need a data structure to represent the memory 
  as a mapping from variable names to values (like integers or booleans).*)

  (* DEFINING THE ENVIRONMENT (THE MEMORY) AS A MAP *)
module StringMap = Map.Make(String)
(* Accepted types in memory: int and bool *)
type mem_value =
| MemInt of int
| MemBool of bool

(* Type memory *)
type memory = mem_value StringMap.t

(* Find a variable in the memory 
e.g. let value_x = StringMap.find "x" mem *)
let lookup (x : string) (m : memory) : mem_value =
  try StringMap.find x m
  with Not_found -> failwith ("Variable is not in the memory: " ^ x)

(* Update the memory with a new variable binding 
e.g. *)
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

  (* eval_expr: This function recursively evaluates expressions:
Integer and Boolean: Simply return MemInt and MemBool.
Variable: Look up the variable in the environment (lookup function).
Arithmetic expressions (Add, Sub, Mul): Evaluate both subexpressions and return their sum, difference, or product.
Comparisons (LessThan): Compare the two subexpressions and return a BoolVal.
Logical operations (And, Not): Evaluate the subexpressions and return the result of the logical operation.
Type checking: The function checks that arithmetic operations are only performed on integers and logical operations 
on booleans. If there’s a type mismatch, it raises a runtime error. *)


(* Evaluating commands *)
let rec exec_cmd (c : exp) (m1 : memory) : memory =
  match c with
  | Skip -> m1  (* skip does nothing *)
  | Assign (x, e) -> 
      let v = eval_expr e m1 in
      (match v with
        | MemInt _ | MemBool _ -> update x v m1  (* Update the variable in the memory *)
        | _ -> failwith ("Pattern matching failure: assigning an unsupported type to variable " ^ x))
  | Seq (c1, c2) -> 
      let m2 = exec_cmd c1 m1 in
      exec_cmd c2 m2  (* Execute the second command after the first *)
  | If (b, c1, c2) -> 
    let v = eval_expr b m1 in
      (match eval_expr b m1 with
      | MemBool true -> exec_cmd c1 m1  (* Execute then-branch *)
      | MemBool false -> exec_cmd c2 m1  (* Execute else-branch *)
      | MemInt _ -> failwith "Pattern matching failure: expected boolean in 'if' condition, but got an integer"
      | _ -> failwith "Pattern matching failure in if condition")
  | While (b, c1) -> 
      let rec loop m2 =
        match eval_expr b m2 with
        | MemBool true -> loop (exec_cmd c1 m2)  (* Continue the loop *)
        | MemBool false -> m2  (* Exit the loop *)
        | MemInt _ -> failwith "Pattern matching failure: expected boolean in 'while' condition, but got an integer"
        | _ -> failwith "Type error in while condition"
      in
      loop m1
  | _ -> failwith "Invalid command in exec_cmd"

(* exec_cmd: This function recursively executes commands:
Skip: Does nothing and returns the current memory unchanged.
Assign: Evaluates the expression on the right-hand side, then updates the memory with the new value for the variable.
Seq: Executes the first command, then the second, passing the updated memory from the first command into the second.
If: Evaluates the condition (which must be a boolean). If true, it executes the "then" command, otherwise the "else" command.
While: Evaluates the condition (which must be a boolean). If true, it executes the body of the loop and repeats. If false, it exits the loop.
*)

(* Executing a full program *)
let exec_program (p : exp) (i : int) : int =
  match p with
  | Program (input, output, body) ->
      let m1 = StringMap.add input (MemInt i) StringMap.empty in
      let m2 = exec_cmd body m1 in
      (match lookup output m2 with
      | MemInt v -> v
      | _ -> failwith "Output variable is not an integer")
  | _ -> failwith "Not a valid program"

  (*
  exec_program: This function initializes the memory with the input variable and its value, executes the program body, and finally looks up the output variable in the final memory state to return the result.
  *)