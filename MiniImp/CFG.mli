(* Each node represents a basic block (sequence of commands).
Edges are represented by the edges list in the node *)
 
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

(* Creating a node with statements and list of successors *)
val create_node : MiniImp.cmd list -> node_id list -> node

(* CFG construction for a command (cmd) *)
val cmd_node : MiniImp.cmd -> node_id -> cfg -> node_id * cfg

(* CFG construction for a complete program *)
val build_cfg : MiniImp.program -> cfg



(* DATAFLOW ANALYSIS *)
(* Checking if there are used variables before without inizialization *)
val check_uninitialized_variables : cfg -> string -> bool 




