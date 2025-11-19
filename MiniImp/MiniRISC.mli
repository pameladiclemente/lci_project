(* Project Fragment:
• Write a module for MiniRISC (syntax and simple statements,
the semantics is not required)
• Implement a translation from MiniImp CFG to MiniRISC CFG
• Implement a translation from MiniRISC CFG to MiniRISC
*)
open MiniImp
open CFG

(* Identifiers for registers and labels *)
type register = string 
type label = string


(* Binary, unary, and immediate operations employed by MiniRISC instructions... *)
(* ... between virtual registers (e.g., add r1 r2 => r3). *)
type brop = 
  | Add               
  | Sub               
  | Mul               
  | Div               
  | Mod               
  | And              
  | Or
  | LessThan         
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Equal
             
(* ... between a register and an immediate constant (e.g., addI r1 10 => r3). *)
type biop = 
  | AddInt  
  | SubInt  
  | MultInt 
  | DivInt  
  | ModInt  
  | AndInt  
  | OrInt   
    
(* ... between virtual registers (e.g., not r1 => r2). *)
type urop = 
  | Not     
  | Copy                       

(* MiniRISC instructions employing defined typed of operations *)
type instruction =
  | Nop                                             (* nop *)
  | Brop of brop * register * register * register   (* brop r r => r *)
  | Biop of biop * register * int * register        (* biop r n => r *)
  | Urop of urop * register * register              (* urop r => r *)
  | Load of register * register                     (* load r => r *)
  | LoadI of int * register                         (* loadI n => r *)
  | Store of register * register                    (* store r => r *)
  | Jump of label                                   (* jump l *)
  | CJump of register * label * label               (* cjump r l l *)

(* Node in MiniRISC CFG, contains:
  - A label (unique)
  - A list of instructions
  - A list of successors *)
type labelled_block = {
  label: label;
  statements: instruction list;
  edges: label list;
}

(* The MiniRISC CFG, contains:
  - A list of labelled blocks (previously defined)
  - An entry node 
  - A terminal node *)
type risc_cfg = {
  blocks: (label * labelled_block) list;
  entry_node: label;
  terminal_node: label;
}

(* Function to translate a MiniImp CFG node into a MiniRISC block *)
val node_to_block : CFG.node -> labelled_block

(* Translation of a MiniImp CFG into a MiniRISC CFG *)
val riscfg_in_assembly : risc_cfg -> string

(* Translation of a MiniImp CFG into a MiniRISC CFG *)
val impcfg_in_riscfg : CFG.cfg -> risc_cfg

(* Target Code Generation
Project Fragment: 
- Implement a translation from MiniRISC CFG to MiniRISC for a target architecture: 
the number of registers must be an integerparameter (must work for n >= 4) 
*)

(* Function to translate a MiniRISC CFG to a target MiniRISC CFG with n>=4 registers *)
val translate_to_target : risc_cfg -> int -> risc_cfg

