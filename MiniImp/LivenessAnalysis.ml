(* Project Fragment:
Write a module containing a function for computing liveness analysis on MiniRISC CFGs *)

open MiniRISC
module StringSet = Set.Make (String)

(* Phase 1: helper function to extract use and def registers from a single MiniRISC instruction. *)
let use_def_registers (instr : instruction) : StringSet.t * StringSet.t =
  let use_list, def_list =
    match instr with
    | Brop (_, r1, r2, r3) -> ([ r1; r2 ], [ r3 ])
    | Biop (_, r1, _, r2) -> ([ r1 ], [ r2 ])
    | Urop (_, r1, r2) -> ([ r1 ], [ r2 ])
    | Load (r1, r2) -> ([ r1 ], [ r2 ])
    | LoadI (_, r) -> ([], [ r ])
    | Store (r1, r2) -> ([ r1; r2 ], [])
    | CJump (r, _, _) -> ([ r ], [])
    | Nop | Jump _ -> ([], [])
  in
  (StringSet.of_list use_list, StringSet.of_list def_list)

(* Liveness Analysis computation, LFP 
Given in input a MiniRISC CFG and the name of the output register, 
computes the liveness analysis and returns a hastable that contains for each label
the set of live-in and live-out registers. *)
let liveness (cfg : risc_cfg) (output : string) :
    (label, StringSet.t * StringSet.t) Hashtbl.t =
  (* Phase 2: Initialization for the Least Fixpoint (LFP) 
  Create the hashtable that will store final output; 
  it is initialized with empty for each label in the MiniRISC CFG. *)
  let liveness_table = Hashtbl.create (List.length cfg.blocks) in
  List.iter
    (fun (label, _) ->
      Hashtbl.add liveness_table label (StringSet.empty, StringSet.empty))
    cfg.blocks;

  (* Terminal node initialization: 
  its live-in contains the output variable, its live-out is empty *)
  let terminal_live_in = StringSet.singleton output in
  let _, terminal_live_out = Hashtbl.find liveness_table cfg.terminal_node in
  Hashtbl.replace liveness_table cfg.terminal_node
    (terminal_live_in, terminal_live_out);

  (* Phase 3: Iterative Fixpoint Calculation
  As for the Defined Variables Analysis, we employ a 'changed' flag to monitor stability 
  and the approach is iterative as well but computing the Least Fixpoint *)
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (label, block) ->
        (* Retrieve old live_in and live_out for the block; this is necessary to check for changes later *)
        let old_live_in, old_live_out = Hashtbl.find liveness_table label in
        (* Compute new_live_out(L) = ∪ live_in(S) for each successor S of L *)
        let new_live_out =
          List.fold_left
            (fun computed_live_registers successor_label ->
              let succesor_live_in, _ =
                Hashtbl.find liveness_table successor_label
              in
              StringSet.union computed_live_registers succesor_live_in)
            StringSet.empty block.edges
        in
        (* Compute use(L) and def(L) for the block necessary for new_live_in(L);
      use(L) formula: registers used in L before any definition in L,
      def(L) formula: registers defined in L *)
        let block_use_reg, block_def_reg =
          List.fold_right
            (fun instruction (computed_use_reg, computed_def_reg) ->
              let instruction_use_reg, instruction_def_reg =
                use_def_registers instruction
              in
              (* Compute use registers and def registers for the block *)
              let new_use_reg =
                StringSet.union instruction_use_reg
                  (StringSet.diff computed_use_reg instruction_def_reg)
              in
              let new_def_reg =
                StringSet.union instruction_def_reg computed_def_reg
              in
              (new_use_reg, new_def_reg))
            block.statements
            (StringSet.empty, StringSet.empty)
        in
        (* Compute new_live_in(L) = use(L) ∪ (new_live_out(L) - def(L));
      before compute it for all blocks, then for the terminal one *)
        let new_live_in =
          StringSet.union block_use_reg
            (StringSet.diff new_live_out block_def_reg)
        in
        let final_new_live_in =
          if label = cfg.terminal_node then terminal_live_in else new_live_in
        in
        (* Phase 4: Stability Check and Return *)
        if
          not
            (StringSet.equal old_live_in final_new_live_in
            && StringSet.equal old_live_out new_live_out)
        then (
          Hashtbl.replace liveness_table label (final_new_live_in, new_live_out);
          changed := true))
      cfg.blocks
  done;
  liveness_table

(* Target Code Generation
Project Fragment:
- Implement an optimization procedure from MiniRISC CFG to MiniRISC CFG 
that tries to reduce the number of registers exploiting the liveness analysis
*)

(* Type for interference graph is a list of (register, neighbors) *)
type interference_graph = (register * StringSet.t) list

(* Type for coloring is a list of (virtual_register, physical_register) where physical_register is the assigned color *)
type coloring = (register * register) list

(* Helper function: 
it adds an edge between two registers in the interference graph adding each other as neighbors. *)
let add_edge (r1 : register) (r2 : register)
    (interference_graph : interference_graph) : interference_graph =
  if r1 = r2 then interference_graph
  else
    (* Helper function: updates neighbors for a register *)
    let rec update_neighbors (interference_graph : interference_graph)
        (register : register) (register_neighbor : register) :
        interference_graph =
      match interference_graph with
      (* Empty means we didn't find no register, so we add a new entry *)
      | [] -> [ (register, StringSet.singleton register_neighbor) ]
      (* If not empty, check the remaining list and see if we find the register;
          if we do, we add the neighbor to its set of neighbors, 
          otherwise, we continue searching *)
      | (r, neighbors) :: remaining ->
          if r = register then
            (r, StringSet.add register_neighbor neighbors) :: remaining
          else
            (r, neighbors)
            :: update_neighbors remaining register register_neighbor
    in
    (* Add r2 as neighbor of r1 and r1 as neighbor of r2 *)
    let middle_interference_graph = update_neighbors interference_graph r1 r2 in
    update_neighbors middle_interference_graph r2 r1

(* Function to build the interference graph from the CFG and the liveness table *)
let build_interference_graph (cfg : risc_cfg)
    (liveness_table : (label, StringSet.t * StringSet.t) Hashtbl.t)
    (output_var : string) : interference_graph * StringSet.t =
  let graph = ref [] in
  let all_registers = ref StringSet.empty in

  (* Helper function:
  when a register is met, add it to 'all_registers' set *)
  let add_node register =
    all_registers := StringSet.add register !all_registers
  in

  (* For each block, apply the two interference rules *)
  List.iter
    (fun (label, block) ->
      (* Interference Rule 1: 
    All registers that are simultaneously 'live-out' from a block interfere with each other. 
    To implement this, we first get the live-out set from the liveness table, 
    then for each pair of registers we add an edge between them in the interference graph. *)
      let live_in, live_out = Hashtbl.find liveness_table label in
      let live_out_list = StringSet.elements live_out in
      List.iter add_node live_out_list;

      List.iter
        (fun r1 ->
          List.iter (fun r2 -> graph := add_edge r1 r2 !graph) live_out_list)
        live_out_list;

      (* Interference Rule 2:
    If an instruction defines a register r_def, then r_def interferes with all registers in the live-out set. 
    To implement this, we iterate over each instruction in the block and for each defined register,
    we add egdes. *)
      List.iter
        (fun instr ->
          let _, def_set = use_def_registers instr in
          StringSet.iter
            (fun def_reg ->
              add_node def_reg;
              List.iter
                (fun live_reg -> graph := add_edge def_reg live_reg !graph)
                live_out_list)
            def_set)
        block.statements)
    cfg.blocks;

  (* Add the output variable as a node in the graph if not already present *)
  add_node output_var;

  (!graph, !all_registers)

(* Function to color the interference graph and produce a coloring map, 
where the objective is to minimize the number of physical registers used merging virtual registers through coloring: 
if a color is assigned to a virtual register, all virtual registers with the same color are merged into the same physical register. 
Interfering nodes cannot share the same color. *)
let color_graph (graph : interference_graph) (registers : StringSet.t) :
    coloring =
  let coloring = ref [] in
  let available_colors = List.init 100 (fun i -> "R" ^ string_of_int i) in

  (* This loop iterates over registers to assign colors. 
  For a register, obtain its neighbors and determines which colors are already employed. 
  Then, assign the first available color not used by its neighbors. 
  If no colors are available, assign the register to itself: it cannot be merged. *)
  StringSet.iter
    (fun reg ->
      let neighbors_set =
        List.assoc_opt reg graph |> Option.value ~default:StringSet.empty
      in
      let used_colors_set =
        StringSet.fold
          (fun neighbor acc_set ->
            match List.assoc_opt neighbor !coloring with
            | Some color -> StringSet.add color acc_set
            | None -> acc_set)
          neighbors_set StringSet.empty
      in

      match
        List.find_opt
          (fun color -> not (StringSet.mem color used_colors_set))
          available_colors
      with
      | Some color -> coloring := (reg, color) :: !coloring
      | None -> coloring := (reg, reg) :: !coloring)
    registers;

  !coloring

(* Function to optimize the CFG using the coloring map *)
let optimize_cfg (cfg : risc_cfg) (coloring : coloring) (output_var : string) :
    risc_cfg =
  (* Find the color for a given register, defaulting to itself if not found *)
  let find_color register =
    List.assoc_opt register coloring |> Option.value ~default:register
  in

  (* Handle output variable mapping *)
  let output_color = find_color output_var in
  let coloring_with_output =
    (output_var, output_color) :: List.remove_assoc output_var coloring
  in
  let find_color register =
    List.assoc_opt register coloring_with_output
    |> Option.value ~default:register
  in

  (* Function to rewrite instructions based on coloring *)
  let rewrite_instruction instr =
    match instr with
    | Brop (op, r1, r2, r3) ->
        Brop (op, find_color r1, find_color r2, find_color r3)
    | Biop (op, r1, n, r2) -> Biop (op, find_color r1, n, find_color r2)
    | Urop (op, r1, r2) -> Urop (op, find_color r1, find_color r2)
    | Load (r1, r2) -> Load (find_color r1, find_color r2)
    | LoadI (n, r) -> LoadI (n, find_color r)
    | Store (r1, r2) -> Store (find_color r1, find_color r2)
    | CJump (r, l1, l2) -> CJump (find_color r, l1, l2)
    | Nop | Jump _ -> instr
  in

  (* Update CFG with rewritten optimized instructions *)
  let new_blocks =
    List.map
      (fun (label, block) ->
        let new_statements = List.map rewrite_instruction block.statements in
        (label, { block with statements = new_statements }))
      cfg.blocks
  in
  { cfg with blocks = new_blocks }

(* Optimizing function called by Compiler.ml *)
let optimize_registers (cfg : risc_cfg) (output_var : string) : risc_cfg =
  Printf.printf "Beginning of Liveness Analysis...\n";
  let liveness_table = liveness cfg output_var in

  Printf.printf "Building Interference Graph...\n";
  let graph, registers =
    build_interference_graph cfg liveness_table output_var
  in

  Printf.printf "Coloring...\n";
  let coloring = color_graph graph registers in

  Printf.printf "Rewriting CFG...\n";
  optimize_cfg cfg coloring output_var
