(* DataFlow Analysis
Project Fragment:
Write a module containing a function for computing liveness
analysis on MiniRISC CFGs
*)

open MiniRISC

(*  Defines type 't' for a string set used to represent registers. *)
module StringSet : Set.S with type elt = string

(* Liveness Analysis computation, LFP *)
val liveness : risc_cfg -> string -> (MiniRISC.label, Set.Make(String).t * Set.Make(String).t) Hashtbl.t


(* Target Code Generation
Project Fragment:
- Implement an optimization procedure from MiniRISC CFG to MiniRISC CFG that tries to reduce the number of registers
exploiting the liveness analysis *)
val optimize_registers : risc_cfg -> string -> risc_cfg
