(* Project Fragment:
1. Define a module for control flow graphs
2. Define a function that given a MiniImp program (AST) returns its CFG 
*) 
 
(* Unique ID for node *)
type node_id = int

(* CFG node *)
type node = {
  id: node_id;                       (* Node ID *)
  statements: MiniImp.cmd list;      (* Associated commands to node *)
  edges: node_id list;               (* Successor(s) list *)
}

(* CFG *)
type cfg = {
  nodes: (node_id * node) list;      (* Node list *)
  entry_node: node_id;               (* Entry node *)
  terminal_node: node_id;            (* Exit node *)
}

(* Creation of a new unique identifier for a node *)
val new_node_id : unit -> node_id

(* CFG construction for a complete program *)
val build_cfg : MiniImp.program -> cfg

(* Function for computing defined variables *)
val defined_variables : cfg -> string -> bool 




