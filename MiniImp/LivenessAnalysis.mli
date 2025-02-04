module LivenessAnalysis : sig
  val compute_liveness : MiniRISC.risc_cfg -> (MiniRISC.label, Set.Make(String).t * Set.Make(String).t) Hashtbl.t
end
