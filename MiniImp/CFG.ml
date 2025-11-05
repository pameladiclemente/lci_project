module StringSet = Set.Make(String)

(* Unique ID for node *)
type node_id = int 

(* CFG node *)
type node = {
  id: node_id;                       (* Node ID *)
  statements: MiniImp.cmd list;      (* Associated commands to node *)
  edges: node_id list;               (* Successor(s) *)
}

(* CFG *) 
type cfg = {
  nodes: (node_id * node) list;      (* Node list *)
  entry_node: node_id;               (* Entry node *)
  terminal_node: node_id;            (* Exit node *)
}

(* Creation of a new unique identifier for a node *)
let node_counter = ref 0 
let new_node_id () = 
  let id = !node_counter in 
  incr node_counter; 
  id 

(* Creating a node with unique ID, associated statements and edges *)
let create_node statements edges = 
  { id = new_node_id (); statements = statements; edges = edges } 

(* CFG construction for a command *)
let rec cmd_node (cmd : MiniImp.cmd) (exit_node : node_id) (cfg : cfg) : node_id * cfg = 
  match cmd with
  | Skip -> (* Skip node points to exit_node *)
      let node = create_node [Skip] [exit_node] in
      (node.id, { cfg with nodes = (node.id, node) :: cfg.nodes }) 
  | Assign (x, a) -> (* Assignment node points to exit_node *)
      let node = create_node [Assign (x, a)] [exit_node] in
      (node.id, { cfg with nodes = (node.id, node) :: cfg.nodes })
  | Seq (c1, c2) -> (* Sequence node: c2 constructed first pointing to exit_node, then c1 points to c2's entry *)
      let c2_entry, cfg_after_c2 = cmd_node c2 exit_node cfg in 
      cmd_node c1 c2_entry cfg_after_c2 
  | If (cond, then_cmd, else_cmd) -> (* If node: then and else branches constructed both pointing to exit_node then conditional node points to both *)
      let then_entry, cfg_after_then = cmd_node then_cmd exit_node cfg in
      let else_entry, cfg_after_else = cmd_node else_cmd exit_node cfg_after_then in
      let cond_node = create_node [If (cond, Skip, Skip)] [then_entry; else_entry] in
      (cond_node.id, { cfg_after_else with nodes = (cond_node.id, cond_node) :: cfg_after_else.nodes })
  | While (cond, body_cmd) -> (* While node: body constructed pointing to cond_node, conditional node then points to body and exit_node *)
      let loop_node = new_node_id () in (* Fictitious node for loop entry *)
      let body_entry, cfg_after_body = cmd_node body_cmd loop_node cfg in
      let cond_node = create_node [While (cond, Skip)] [body_entry; exit_node] in
      let loop_node_entry = { id = loop_node; statements = []; edges = [cond_node.id] } in
      (cond_node.id, { cfg_after_body with nodes = (cond_node.id, cond_node) :: (loop_node, loop_node_entry) :: cfg_after_body.nodes })


(* CFG construction for a complete program *)
let build_cfg (program : MiniImp.program) : cfg =
  match program with
  | MiniImp.Program (_, _, body_cmd) -> 
      let exit_node = new_node_id () in
      let entry_node, cfg = cmd_node body_cmd exit_node { nodes = []; entry_node = 0; terminal_node = 0 } in
      { cfg with entry_node = entry_node; terminal_node = exit_node }




(* Dataflow Analysis: computing defined variables with GFP *)

(* Phase 1: helper functions... 
   ... to extract variables used by a_exp or b_exp *)
let rec vars_in_a_exp (a_exp : MiniImp.a_exp) : StringSet.t =
  match a_exp with
  | Integer n -> StringSet.empty
  | Variable v -> StringSet.singleton v
  | Add (a1, a2) 
  | Sub (a1, a2) 
  | Mul (a1, a2) 
  | Div (a1, a2) 
  | Mod (a1, a2) -> StringSet.union (vars_in_a_exp a1) (vars_in_a_exp a2)
  | NotInt n -> vars_in_a_exp n

let rec vars_in_b_exp (b_exp : MiniImp.b_exp) : StringSet.t =
  match b_exp with
  | Boolean b -> StringSet.empty
  | And (b1, b2) 
  | Or (b1, b2) -> StringSet.union (vars_in_b_exp b1) (vars_in_b_exp b2)
  | NotBool b -> vars_in_b_exp b
  | LessThan (a1, a2) 
  | LessThanEqual (a1, a2) 
  | GreaterThan (a1, a2) 
  | GreaterThanEqual (a1, a2) 
  | Equal (a1, a2) -> StringSet.union (vars_in_a_exp a1) (vars_in_a_exp a2)

(* ... to extract variables used and defined in cmd *)
let vars_in_cmd (cmd : MiniImp.cmd) : (StringSet.t * StringSet.t) =
  match cmd with
  | Assign (v, a_exp) -> (vars_in_a_exp a_exp, StringSet.singleton v)
  | If (b_exp, cmd1, cmd2) -> (vars_in_b_exp b_exp, StringSet.empty)
  | While (b_exp, cmd) -> (vars_in_b_exp b_exp, StringSet.empty)
  | Skip -> (StringSet.empty, StringSet.empty)
  | Seq (cmd1, cmd2) -> (StringSet.empty, StringSet.empty)


(* Function for computing defined variables *)
let defined_variables (cfg : cfg) (input : string) : bool =
  
  (* Phase 2: Computation of auxiliary structures *)
  (* Predecessor map *)
  let predecessors = Hashtbl.create (List.length cfg.nodes) in

  List.iter (fun (id, node) ->
    List.iter (fun next_id ->
      let current_predecessors = 
        try Hashtbl.find predecessors next_id 
        with Not_found -> [] in 
        Hashtbl.replace predecessors next_id (id :: current_predecessors)
    ) node.edges
  ) cfg.nodes;

  (* Local Definition map, defs(L) *)
  let block_definitions = Hashtbl.create (List.length cfg.nodes) in
  List.iter (fun (id, node) ->
    let definitions = List.fold_left (fun computed_definitions cmd ->
      let _, definitions_in_cmd = vars_in_cmd cmd in 
      StringSet.union computed_definitions definitions_in_cmd
    ) StringSet.empty node.statements in
    Hashtbl.add block_definitions id definitions
  ) cfg.nodes;

  (* Phase 3: Initialization for the Greatest Fixpoint (GFP) *)
  let dvin_table = Hashtbl.create (List.length cfg.nodes) in
  let dvout_table = Hashtbl.create (List.length cfg.nodes) in

  let top = Hashtbl.fold (fun _ definitions computed_definitions -> 
    StringSet.union computed_definitions definitions
    ) block_definitions (StringSet.singleton input) in

  List.iter (fun (id, _) ->
    (* Entry node *)
    if id = cfg.entry_node then (
      Hashtbl.add dvin_table id (StringSet.singleton input);
      Hashtbl.add dvout_table id (StringSet.union (StringSet.singleton input) (Hashtbl.find block_definitions id))
    (* Other nodes *)
      ) else (
      Hashtbl.add dvin_table id StringSet.empty;
      Hashtbl.add dvout_table id top 
    )
  ) cfg.nodes;

  (* Phase 4: Iterative Fixpoint Computation *)
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (id, _) ->
      (* Entry node *)
      if id <> cfg.entry_node then (
      (* Other nodes *)
        let old_dvout = Hashtbl.find dvout_table id in
        (* new_dvin(L) computation *)
        let predecessors_ids = try Hashtbl.find predecessors id with Not_found -> [] in
        let new_dvin =
          match predecessors_ids with
          (* Se un nodo non ha predecessori (e non è l'entry), è irraggiungibile. Nessuna variabile è sicuramente definita al suo ingresso. *)
          | [] -> StringSet.empty 
          (* Se ci sono predecessori... *)
          | h :: t ->
              (* Inizializza l'intersezione con il dvout del primo predecessore *)
              let first_dvout = Hashtbl.find dvout_table h in
              (* Interseca con i dvout di tutti gli altri predecessori *)
              List.fold_left (fun acc pred_id ->
                StringSet.inter acc (Hashtbl.find dvout_table pred_id)
              ) first_dvout t
        in
        Hashtbl.replace dvin_table id new_dvin;
        let block_def_set = Hashtbl.find block_definitions id in
        let new_dvout = StringSet.union new_dvin block_def_set in

        (* Stability check *)
        if not (StringSet.equal old_dvout new_dvout) then (
          Hashtbl.replace dvout_table id new_dvout;
        changed := true
      )
    )
  ) cfg.nodes
done;
(* When in here, we reached the fixpoint ('dvin_table' and 'dvout_table' are stable, 'changed' is false) *)

(* Phase 5: Final Checking and Error Verification *)
let errors = ref false in
List.iter (fun (id, node) ->
  let current_defs = ref (Hashtbl.find dvin_table id) in
  List.iter (fun cmd ->
    let use_vars, def_vars = vars_in_cmd cmd in
    List.iter (fun v ->
      if not (StringSet.mem v !current_defs) then (
        let cmd_str = match cmd with
          | Assign (x, _) -> "Assign(" ^ x ^ ", ...)"
          | If _ -> "If(...)" | While _ -> "While(...)" | _ -> "Cmd"
        in
        Printf.printf "Error found in block L%d: var '%s' used in '%s' but not surely initialized\n" 
          id v cmd_str;
        errors := true
      )
    ) (StringSet.elements use_vars); 
    (* Update *)
    current_defs := StringSet.union !current_defs def_vars
  ) node.statements
) cfg.nodes;

not !errors

