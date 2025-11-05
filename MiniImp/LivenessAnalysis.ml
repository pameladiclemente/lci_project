open MiniRISC

(* Dataflow Analysis: computing live variables with LFP *)

module StringSet = Set.Make(String)

(* Phase 1: Helper function to extract use and def registers from a single MiniRISC instruction. *)
let use_def_registers (instr : MiniRISC.instruction) : (StringSet.t * StringSet.t) =
  let (use_list, def_list) = match instr with
    | MiniRISC.Brop (_, r1, r2, r3) -> ([r1; r2], [r3])
    | MiniRISC.Biop (_, r1, _, r2) -> ([r1], [r2])
    | MiniRISC.Urop (_, r1, r2) -> ([r1], [r2])
    | MiniRISC.Load (r1, r2) -> ([r1], [r2])
    | MiniRISC.LoadI (_, r) -> ([], [r])
    | MiniRISC.Store (r1, r2) -> ([r1; r2], [])
    | MiniRISC.CJump (r, _, _) -> ([r], [])
    | MiniRISC.Nop | MiniRISC.Jump _ -> ([], [])
  in
  (StringSet.of_list use_list, StringSet.of_list def_list)

(* Liveness Analysis computation, LFP *)
let liveness (cfg : risc_cfg) (output : string) : (label, StringSet.t * StringSet.t) Hashtbl.t =
  
  (* Phase 2: Initialization for the Least Fixpoint (LFP) *)
  let liveness_table = Hashtbl.create (List.length cfg.blocks) in
  List.iter (fun (label, _) ->
    Hashtbl.add liveness_table label (StringSet.empty, StringSet.empty)
  ) cfg.blocks;

  (* Terminal node initialization: assume that the output register is live-in. *)
  let terminal_live_in = StringSet.singleton output in
  let _, terminal_live_out = Hashtbl.find liveness_table cfg.terminal_node in
  Hashtbl.replace liveness_table cfg.terminal_node (terminal_live_in, terminal_live_out);


  (* Iterative Fixpoint Calculation *)
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (label, block) ->
      (* Retrieve old live_in and live_out for the block this is necessary to check for changes later *)
      let old_live_in, old_live_out = Hashtbl.find liveness_table label in
      (* Compute new_live_out(L) = ∪ live_in(S) for each successor S of L *)
      let new_live_out =
        List.fold_left (fun computed_live_registers successor_label ->
          let succesor_live_in, _ = Hashtbl.find liveness_table successor_label in
          StringSet.union computed_live_registers succesor_live_in
        ) StringSet.empty block.edges 
      in
      (* Compute use(L) and def(L) for the block *)
      let block_use_reg, block_def_reg = 
        List.fold_right (fun instruction (computed_use_reg, computed_def_reg) ->
          let instruction_use_reg, instruction_def_reg = use_def_registers instruction in
          (* Compute use registers and def registers for the block *)
          let new_use_reg = StringSet.union instruction_use_reg (StringSet.diff computed_use_reg instruction_def_reg) in
          let new_def_reg = StringSet.union instruction_def_reg computed_def_reg in
          (new_use_reg, new_def_reg)
        ) block.statements (StringSet.empty, StringSet.empty) 
      in
      (* Compute new_live_in(L) = use(L) ∪ (new_live_out(L) - def(L)) *)
      let new_live_in = 
        StringSet.union block_use_reg (StringSet.diff new_live_out block_def_reg)
      in
      let final_new_live_in = 
        if label = cfg.terminal_node then
          terminal_live_in
        else
          new_live_in
      in
      (* Phase 3: Stability Check *)
      if not (StringSet.equal old_live_in final_new_live_in && 
              StringSet.equal old_live_out new_live_out) 
      then (
        Hashtbl.replace liveness_table label (final_new_live_in, new_live_out);
        changed := true
      )
    ) cfg.blocks;
  done;
  liveness_table

