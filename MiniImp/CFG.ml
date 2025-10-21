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




(*
(* DATAFLOW ANALYSIS 


Usiamo un Hashtbl (defined_vars) per tracciare quali variabili sono definite.
Aggiungiamo la variabile di input (input_var) come inizializzata.
check_instr analizza ogni comando:
Se è un assegnamento (Assign), controlla che le variabili usate siano inizializzate.
Se è una condizione (If, While), verifica che tutte le variabili nei confronti siano definite.
Dopo un'assegnazione, aggiungiamo la variabile all'insieme di quelle inizializzate.
Iteriamo su tutti i blocchi del CFG.
Se troviamo errori, stampiamo un messaggio e restituiamo false.*)

(* Controllo che nessuna variabile sia usata prima di essere inizializzata *)
let check_uninitialized_variables (cfg : cfg) (input_var : string) : bool =
  let defined_vars = Hashtbl.create 50 in
  let errors = ref false in

  (* Aggiungiamo la variabile di input come già inizializzata *)
  Hashtbl.add defined_vars input_var true;

  (* Funzione per controllare tutte le variabili usate in un'espressione aritmetica *)
  let rec vars_in_a_exp = function
    | MiniImp.Integer _ -> []
    | Variable v -> [v]
    | Add (a1, a2) | Sub (a1, a2) | Mul (a1, a2) | Div (a1, a2) | Mod (a1, a2) -> vars_in_a_exp a1 @ vars_in_a_exp a2
  in

  (* Funzione per controllare tutte le variabili usate in un'espressione booleana *)
  let rec vars_in_b_exp = function
    | MiniImp.Boolean _ -> []
    | And (b1, b2) | Or (b1, b2) -> vars_in_b_exp b1 @ vars_in_b_exp b2
    | Not b -> vars_in_b_exp b
    | LessThan (a1, a2) | LessThanEqual (a1, a2) | GreaterThan (a1, a2) | GreaterThanEqual (a1, a2) | Equal (a1, a2) ->
        vars_in_a_exp a1 @ vars_in_a_exp a2
  in

  (* Funzione per controllare una singola istruzione *)
  let check_instr instr =
    match instr with
    | MiniImp.Assign (x, a_exp) ->
        List.iter (fun v ->
          if not (Hashtbl.mem defined_vars v) then (
            Printf.printf "Errore: La variabile %s è usata prima di essere inizializzata!\n" v;
            errors := true
          )
        ) (vars_in_a_exp a_exp);
        Hashtbl.replace defined_vars x true

    | If (b_exp, _, _) | While (b_exp, _) ->
        List.iter (fun v ->
          if not (Hashtbl.mem defined_vars v) then (
            Printf.printf "Errore: La variabile %s è usata prima di essere inizializzata!\n" v;
            errors := true
          )
        ) (vars_in_b_exp b_exp)

    | _ -> ()
  in

  (* Controlliamo ogni blocco nel CFG *)
  List.iter (fun (_, block) ->
    List.iter check_instr block.code
  ) cfg.nodes;

  not !errors  (* Ritorna true se tutto è corretto, false se ci sono errori *)


(*Costruzione minimale, massimale o intermedia?
Minimale: Blocchi di base senza nodi inutili.
Massimale: Blocchi singoli per ogni comando.
Intermedia: Ogni nodo rappresenta un comando o una sequenza lineare di comandi.
La mia implementazione è intermedia:

Ogni nodo rappresenta un singolo comando o una sequenza massimale di comandi senza diramazioni.*)

*)