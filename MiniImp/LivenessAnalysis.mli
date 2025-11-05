open MiniRISC
(*  Defines type 't' for a string set used to represent registers. *)
module StringSet : Set.S with type elt = string
(* Liveness Analysis computation, LFP *)
val liveness : risc_cfg -> string -> (MiniRISC.label, Set.Make(String).t * Set.Make(String).t) Hashtbl.t

