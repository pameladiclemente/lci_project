(* Each node represents a basic block (sequence of commands).
Edges are represented by the successors list in the node *)

(* Unique ID for node *)
type node_id = int 

(* CFG node *)
type node = {
  id: node_id;                 (* Node ID *)
  code: MiniImp.cmd list;      (* Associated commands to node *)
  successors: node_id list;    (* Successor(s) list *)
}

(* CFG *)
type cfg = {
  nodes: (node_id * node) list; (* Node list *)
  entry: node_id;               (* Entry node *)
  exit: node_id;                (* Exit node *)
}


(* Creation of a new unique identifier for a node *)
let node_counter = ref 0
let new_node_id () = let id = !node_counter in incr node_counter; id

(* Creating a node with code and list of successors *)
let create_node code successors =
  { id = new_node_id (); code; successors }

(* CFG construction for a command (cmd) *)
let rec build_cmd (cmd : MiniImp.cmd) (exit_node : node_id) (cfg : cfg) : node_id * cfg =
  match cmd with
  | Skip ->
      (* Nodo per Skip con successore exit_node *)
      let node = create_node [Skip] [exit_node] in
      (* Restituisce l'ID del nodo e il CFG aggiornato *)
      (node.id, { cfg with nodes = (node.id, node) :: cfg.nodes })

  | Assign (x, a) ->
      (* Nodo per l'assegnazione con successore exit_node *)
      let node = create_node [Assign (x, a)] [exit_node] in
      (* Restituisce l'ID del nodo e il CFG aggiornato *)
      (node.id, { cfg with nodes = (node.id, node) :: cfg.nodes })

  | Seq (c1, c2) ->
      (* Costruzione ricorsiva del CFG per c2 e uso del suo nodo iniziale come uscita di c1 *)
      let mid_node, cfg' = build_cmd c2 exit_node cfg in
      build_cmd c1 mid_node cfg'

  | If (cond, then_cmd, else_cmd) ->
      (* Costruzione dei rami then e else *)
      let then_exit, cfg1 = build_cmd then_cmd exit_node cfg in
      let else_exit, cfg2 = build_cmd else_cmd exit_node cfg1 in
      (* Nodo condizionale con successori then e else *)
      let cond_node = create_node [If (cond, Skip, Skip)] [then_exit; else_exit] in
      (* Restituisce l'ID del nodo condizionale e il CFG aggiornato *)
      (cond_node.id, { cfg2 with nodes = (cond_node.id, cond_node) :: cfg2.nodes })

  | While (cond, body_cmd) ->
      (* Nodo fittizio per rappresentare l'ingresso del ciclo *)
      let loop_node = new_node_id () in
      (* Costruzione del corpo del ciclo che punta al nodo condizionale *)
      let body_exit, cfg1 = build_cmd body_cmd loop_node cfg in
      (* Nodo condizionale con un arco verso il corpo del ciclo e uno verso l'uscita *)
      let cond_node = create_node [While (cond, Skip)] [body_exit; exit_node] in
      (* Nodo fittizio che punta al nodo condizionale *)
      let loop_node_entry = { id = loop_node; code = []; successors = [cond_node.id] } in
      (* Restituisce l'ID del nodo condizionale e il CFG aggiornato *)
      (cond_node.id, { cfg1 with nodes = (cond_node.id, cond_node) :: (loop_node, loop_node_entry) :: cfg1.nodes })

(* Tipo di ritorno corretto:
Ogni ramo del match restituisce una coppia (node_id, cfg).
Sequenza di comandi (Seq):
La costruzione di c1 utilizza come exit_node il nodo iniziale di c2 restituito dalla costruzione ricorsiva.
Condizionale (If):
Il nodo condizionale ha due successori: l'uscita di then_cmd e quella di else_cmd.
Il exit_node comune è passato a entrambi i rami.
Ciclo (While):
Viene creato un nodo fittizio (loop_node) come ingresso al ciclo.
Il corpo del ciclo punta al nodo condizionale (cond_node), e il nodo condizionale punta sia al corpo sia al exit_node.*)

(* CFG construction for a complete program *)
let build_cfg (program : MiniImp.program) : cfg =
  match program with

  (* Specifiche del PDF:
Il programma è formato da un corpo di comandi (c).
Slide di riferimento: 13.
Sto ignorando la parte def main with input x output y as?
Sì, al momento il CFG rappresenta solo il corpo (body_cmd).
Se necessario, possiamo aggiungere nodi che rappresentano l'inizializzazione e l'output.*)
  | MiniImp.Program (_, _, body_cmd) ->
      let exit_node = new_node_id () in
      let entry_node, cfg = build_cmd body_cmd exit_node { nodes = []; entry = 0; exit = 0 } in
      { cfg with entry = entry_node; exit = exit_node }





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

  (* Funzione per controllare una singola istruzione 
  let check_instr instr =
    match instr with
    | MiniImp.Assign (x, a_exp) ->
        (* Controlliamo che tutte le variabili usate nell'espressione siano inizializzate *)
        let rec vars_in_exp = function
          | MiniImp.Integer _ -> []
          | MiniImp.Variable v -> [v]
          | MiniImp.Add (e1, e2) | MiniImp.Sub (e1, e2)
          | MiniImp.Mul (e1, e2) | MiniImp.Div (e1, e2)
          | MiniImp.Mod (e1, e2) -> vars_in_exp e1 @ vars_in_exp e2
          | MiniImp.Equal (a1, a2) -> vars_in_exp a1 @ vars_in_exp a2
        in
        List.iter (fun v ->
          if not (Hashtbl.mem defined_vars v) then (
            Printf.printf "Errore: La variabile %s è usata prima di essere inizializzata!\n" v;
            errors := true
          )
        ) (vars_in_exp a_exp);
        (* Dopo l'assegnazione, la variabile `x` è inizializzata *)
        Hashtbl.replace defined_vars x true

    | MiniImp.If (b_exp, _, _) | MiniImp.While (b_exp, _) ->
        let rec vars_in_bexp = function
          | MiniImp.Boolean _ -> []
          | MiniImp.And (b1, b2) | MiniImp.Or (b1, b2) -> vars_in_bexp b1 @ vars_in_bexp b2
          | MiniImp.Not b -> vars_in_bexp b
          | MiniImp.LessThan (a1, a2) | MiniImp.LessThanEqual (a1, a2)
          | MiniImp.GreaterThan (a1, a2) | MiniImp.GreaterThanEqual (a1, a2)
        
        in
        List.iter (fun v ->
          if not (Hashtbl.mem defined_vars v) then (
            Printf.printf "Errore: La variabile %s è usata prima di essere inizializzata!\n" v;
            errors := true
          )
        ) (vars_in_bexp b_exp)

    | _ -> ()
  in

  (* Controlliamo ogni blocco nel CFG *)
  List.iter (fun (_, block) ->
    List.iter check_instr block.code
  ) cfg.nodes;

  not !errors  (* Ritorna true se tutto è corretto, false se ci sono errori *)

*)

(*Costruzione minimale, massimale o intermedia?
Minimale: Blocchi di base senza nodi inutili.
Massimale: Blocchi singoli per ogni comando.
Intermedia: Ogni nodo rappresenta un comando o una sequenza lineare di comandi.
La mia implementazione è intermedia:

Ogni nodo rappresenta un singolo comando o una sequenza massimale di comandi senza diramazioni.*)