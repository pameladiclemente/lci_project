(* Each node represents a basic block (sequence of commands).
Edges are represented by the edges list in the node *)

(* Unique ID for node *)
type node_id = int 

(* CFG node *)
type node = {
  id: node_id;                       (* Node ID *)
  statements: MiniImp.cmd list;      (* Associated commands to node *)
  edges: node_id list;               (* Successor(s) list *)
}

(* CFG *) 
type cfg = {
  nodes: (node_id * node) list; (* Node list, I nodi del CFG (N) sono rappresentati come una lista di nodi *)
  entry_node: node_id;               (* Entry node *)
  terminal_node: node_id;                (* Exit node *)
}


(* Creation of a new unique identifier for a node *)
let node_counter = ref 0 (* ref 0 crea un riferimento mutabile, ovvero una variabile che può essere modificata in OCaml*)
let new_node_id () = 
  let id = !node_counter in (* legge il valore della variabile node_counter *)
  incr node_counter; (*  Incrementa node_counter per il prossimo nodo *)
  id (* restituisce il valore vecchio *)

(* Creating a node with statements and list of edges;
Crea un nuovo nodo del CFG con: Un ID univoco generato da new_node_id (),
Una lista di comandi associati, Una lista di successori del nodo *)
let create_node statements edges = (* Funzione che accetta due parametri (statements, edges) e restituisce un record. *)
  { id = new_node_id (); statements = statements; edges = edges } (* Crea un record con i campi id, statements e edges. *)

(* CFG construction for a command (cmd) 
Restituisce l'ID del nodo creato e il CFG aggiornato*)
let rec build_cmd (cmd : MiniImp.cmd) (exit_node : node_id) (cfg : cfg) : node_id * cfg = (* Dichiariamo una funzione ricorsiva con parametri: comando MiniImp da tradurre in CFG, nodo di uscita del comando, CFG corrente*)
  match cmd with
  (* Crea un nodo per rappresentare l'operazione Skip, Il nodo ha un solo successore: exit_node, il nodo di uscita -> Aggiorna il CFG aggiungendo il nodo alla lista cfg.nodes*)
  | Skip ->
      (* Nodo per Skip con successore exit_node *)
      let node = create_node [Skip] [exit_node] in
      (* Restituisce l'ID del nodo e il CFG aggiornato *)
      (node.id, { cfg with nodes = (node.id, node) :: cfg.nodes }) (* Crea un nuovo record cfg, aggiornando solo il campo nodes, :: è l'operatore di cons che aggiunge il nuovo nodo in testa alla lista cfg.nodes.*)

  | Assign (x, a) ->
      (* Nodo per l'assegnazione con successore exit_node *)
      let node = create_node [Assign (x, a)] [exit_node] in
      (* Restituisce l'ID del nodo e il CFG aggiornato *)
      (node.id, { cfg with nodes = (node.id, node) :: cfg.nodes })


      (* Compila prima c2, ottenendo mid_node, il suo primo nodo, e il CFG aggiornato cfg'.
      Usa mid_node come exit_node per c1: in questo modo c1 punta direttamente a c2.
      Compila c1 con mid_node come successore e aggiorna il CFG.*)
  | Seq (c1, c2) ->
      (* Costruzione ricorsiva del CFG per c2 e uso del suo nodo iniziale come uscita di c1 *)
      let mid_node, cfg' = build_cmd c2 exit_node cfg in (*Chiama ricorsivamente build_cmd su c2, con exit_node come uscita, mid_node è l'ID del primo nodo di c2, cfg' è il CFG aggiornato*)
      build_cmd c1 mid_node cfg' (* Compila c1, usando mid_node come suo successore. *)

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
      let loop_node_entry = { id = loop_node; statements = []; edges = [cond_node.id] } in
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

(* CFG construction for a complete program, costruisce un grafo di controllo di flusso (CFG) a partire da un programma MiniImp. *)
let build_cfg (program : MiniImp.program) : cfg =
  match program with

  (* Specifiche del PDF:
Il programma è formato da un corpo di comandi (c).
Slide di riferimento: 13.
Sto ignorando la parte def main with input x output y as?
Sì, al momento il CFG rappresenta solo il corpo (body_cmd).
Se necessario, possiamo aggiungere nodi che rappresentano l'inizializzazione e l'output.*)
  | MiniImp.Program (_, _, body_cmd) -> (*  Il programma MiniImp ha questa struttura: type program = Program of string * string * cmd; Il primo string è il nome della variabile di input (ignorato _).
Il secondo string è il nome della variabile di output (ignorato _). Come da pdf, consideriamo solo il blocco comandi *)
      let exit_node = new_node_id () in
      (* Chiamiamo build_cmd per tradurre body_cmd in CFG. Parametri della chiamata:  Il corpo del programma., Il nodo di uscita, cioè il nodo a cui body_cmd deve puntare quando termina, Un CFG iniziale vuoto*)
      let entry_node, cfg = build_cmd body_cmd exit_node { nodes = []; entry_node = 0; terminal_node = 0 } in
      (* Restituiamo il CFG aggiornato, mantenendo i nodi creati da build_cmd ma cambiando:
        entry_node = entry_node → Il nodo di ingresso ora è il primo nodo creato dal body_cmd.
        terminal_node = exit_node → Il nodo di uscita ora è quello creato all'inizio.*)
      { cfg with entry_node = entry_node; terminal_node = exit_node }





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