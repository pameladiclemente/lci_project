(* CFG.mli *)

open MiniImp
(* Unique ID for node *)
type node_id = int

(* CFG node *)
type node = {
  id: node_id;                 (* Node ID *)
  code: MiniImp.cmd list;      (* Associated commands to node *)
  successors: node_id list;   (* Successor(s) list *)
}

(* CFG *)
type cfg = {
  nodes: (node_id * node) list; (* Node list *)
  entry: node_id;               (* Entry node *)
  exit: node_id;                (* Exit node *)
}

val build_cfg : MiniImp.program -> cfg

(* Creation of a new unique identifier for a node *)
val new_node_id : unit -> node_id

(* Creating a node with code and list of successors *)
val create_node : MiniImp.cmd list -> node_id list -> node

(* CFG construction for a command (cmd) *)
val build_cmd : MiniImp.cmd -> node_id -> cfg -> node_id * cfg

(* CFG construction for a complete program *)
val build_cfg : MiniImp.program -> cfg


(* DATAFLOW ANALYSIS

val check_uninitialized_variables : cfg -> string -> bool
*)

