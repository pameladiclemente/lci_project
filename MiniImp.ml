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

module StringMap = Map.Make(String)
  (* Type for values in memory *)
type mem_value =
| MemInt of int
| MemBool of bool

(* Type for memory *)
type memory = mem_value StringMap.t

(* Find a variable in the memory *)
let lookup (x : string) (m : memory) : mem_value =
  try StringMap.find x m
  with Not_found -> failwith ("Variable is not in the memory: " ^ x)

(* Update the memory with a new variable binding *)
let update (x : string) (v : mem_value) (m : memory) : memory =
  StringMap.add x v m

(* Evaluating expressions *)
