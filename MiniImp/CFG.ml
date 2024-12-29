(* Ogni nodo (node) rappresenta un blocco basilare (sequenza di comandi).
Gli archi sono rappresentati dalla lista successors nel nodo *)


(* Nodo del grafo *)
type node_id = int (* Identificatore univoco *)

type node = {
  id: node_id;         (* ID del nodo *)
  code: MiniImp.cmd list;      (* Comandi associati al nodo *)
  successors: node_id list; (* Lista di successori *)
}

(* Grafo di controllo di flusso *)
type cfg = {
  nodes: (node_id * node) list; (* Lista di nodi con ID e dettagli *)
  entry: node_id;               (* Nodo di ingresso *)
  exit: node_id;                (* Nodo di uscita *)
}

(* Helper per generare ID univoci *)
let node_counter = ref 0
let new_node_id () = let id = !node_counter in incr node_counter; id

(* Creazione di un nodo *)
let create_node code successors =
  { id = new_node_id (); code; successors }


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

(*Costruzione del CFG per un programma*)
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




(*Costruzione minimale, massimale o intermedia?
Minimale: Blocchi di base senza nodi inutili.
Massimale: Blocchi singoli per ogni comando.
Intermedia: Ogni nodo rappresenta un comando o una sequenza lineare di comandi.
La mia implementazione è intermedia:

Ogni nodo rappresenta un singolo comando o una sequenza massimale di comandi senza diramazioni.*)