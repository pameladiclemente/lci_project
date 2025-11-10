open MiniRISC

(* Dataflow Analysis: computing live variables with LFP *)

module StringSet = Set.Make(String)

(* Phase 1: Helper function to extract use and def registers from a single MiniRISC instruction. *)
let use_def_registers (instr : instruction) : (StringSet.t * StringSet.t) =
  let (use_list, def_list) = match instr with
    | Brop (_, r1, r2, r3) -> ([r1; r2], [r3])
    | Biop (_, r1, _, r2) -> ([r1], [r2])
    | Urop (_, r1, r2) -> ([r1], [r2])
    | Load (r1, r2) -> ([r1], [r2])
    | LoadI (_, r) -> ([], [r])
    | Store (r1, r2) -> ([r1; r2], [])
    | CJump (r, _, _) -> ([r], [])
    | Nop | Jump _ -> ([], [])
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




(* Project fragment: 
  Implement an optimization procedure from MiniRISC CFG to MiniRISC CFG that 
  tries to reduce the number of registers exploiting the liveness analysis *)

(* Tipo per il grafo di interferenza:
  È una lista di associazioni (registro, vicini del registro)
*)
type interference_graph = (register * StringSet.t) list

(* Tipo per la colorazione:
  Una lista di associazioni (registro_virtuale, registro_fisico/colore)
  cioè "Prendi il registro virtuale (es. "R7") e mappalo su questo registro fisico/colore (es. "R1")."
  Se "R5" e "R7" non interferiscono, questa mappa potrebbe contenere [("R5", "R1"); ("R7", "R1")], fondendoli entrambi nel registro fisico "R1".
*)
type coloring = (register * register) list

(* Funzione interna per aggiungere un arco tra r1 e r2 al grafo. 
questa funzione deve aggiornare la lista di adiacenza in due punti:
Aggiungere r2 all'elenco dei vicini di r1 e viceversa *)
let add_edge (r1 : register) (r2 : register) (interference_graph : interference_graph) : interference_graph =
  if r1 = r2 then interference_graph
  else
    (* Helper function: updates neighbors for a register *)
    let rec update_neighbors (interference_graph : interference_graph) (register : register) (register_neighbor : register) : interference_graph =
      match interference_graph with
      | [] -> (* lista vuota [] e non c'è register *)
          (* Creiamo una nuova voce. StringSet.singleton neighbor crea un nuovo set che contiene solo "R7". 
          Il risultato è una nuova lista che contiene solo [("R5", {"R7"})]. *)
          [(register, StringSet.singleton register_neighbor)]
      | (r, neighbors) :: remaining -> (* remaining è il resto della lista*)
      (* Il registro r che stiamo guardando è proprio quello che cercavamo (register). *)
          if r = register then
            (r, StringSet.add register_neighbor neighbors) :: remaining (* Creiamo una nuova tupla (r, ...) dove aggiungiamo (StringSet.add) il nostro neighbor all'elenco dei neighbors esistenti. *)
          else
            (* non è r che cercavamo, vai avanti *)
            (r, neighbors) :: (update_neighbors remaining register register_neighbor)
    in
    (* aggiunge r2 ai vicini di r1, la riga dopo il viceversa *)
    let middle_interference_graph = update_neighbors interference_graph r1 r2 in
    update_neighbors middle_interference_graph r2 r1

(* Costruisce il grafo di interferenza *)
let build_interference_graph (cfg : risc_cfg) (liveness_table : (label, StringSet.t * StringSet.t) Hashtbl.t) (output_var : string) : interference_graph * StringSet.t =
  
  (* Inizializziamo i nostri due risultati come ref (riferimenti mutabili). graph è la nostra lista di adiacenza (inizialmente vuota). all_registers è l'insieme di tutti i registri che incontriamo (inizialmente vuoto). Usiamo i ref perché li modificheremo iterativamente dentro un ciclo. *)
  let graph = ref [] in
  let all_registers = ref StringSet.empty in

  (* Una semplice funzione helper. Ogni volta che "vediamo" un registro (r), lo aggiungiamo al nostro set all_registers. StringSet.add gestisce automaticamente i duplicati.*)
  let add_node r =
    all_registers := StringSet.add r !all_registers
  in

  (* Itera su ogni blocco del programma. Per ogni blocco, applicheremo le due regole di interferenza. *)
  List.iter (fun (label, block) ->

    (* Regola di Interferenza 1
    "Tutti i registri che sono simultaneamente 'live-out' da un blocco interferiscono tra loro." *)
    
    (* Prendiamo l'insieme (StringSet.t) dei registri vivi all'uscita (live_out) di questo blocco. *)
    let live_in, live_out = Hashtbl.find liveness_table label in
    
    (* Convertiamo questo insieme in una lista *)
    let live_out_list = StringSet.elements live_out in
    (* Aggiungiamo tutti questi registri al nostro elenco all_registers *)
    List.iter add_node live_out_list;

    (* Questo è un doppio ciclo O(n^2) sulla lista live_out_list. Compara ogni registro con ogni altro registro nella lista. *)
    List.iter (fun r1 ->
      List.iter (fun r2 ->
        (* Per ogni coppia (es. ("R1", "R5"), ("R1", "R7"), ("R5", "R7"), ecc.), 
        aggiunge un arco di interferenza al grafo. add_edge gestisce già il caso r1 = r2. *)
        graph := add_edge r1 r2 !graph
      ) live_out_list
    ) live_out_list;

    (* Regola di Interferenza 2 
    "Se un'istruzione definisce un registro r_def, allora r_def interferisce con tutti i registri che sono 'live-out' da quel blocco." *)
    
    
    (* Scorre ogni istruzione all'interno del blocco corrente. *)
    List.iter (fun instr ->
      (* Per l'istruzione (es. add R2, R3 => R1), ottiene l'insieme dei registri che definisce (def_set), che qui sarebbe {"R1"} *)
      let _, def_set = use_def_registers instr in
      
      (* Itera su ogni registro in def_set *)
      StringSet.iter (fun def_reg ->
        (* Aggiunge il registro definito (es. "R1") al nostro elenco all_registers. *)
        add_node def_reg;
        (* Itera su ogni registro live_reg nella live_out_list *)
        List.iter (fun live_reg ->
          (* Aggiunge un arco tra il registro definito ("R1") e ogni registro vivo all'uscita *)
          graph := add_edge def_reg live_reg !graph
        ) live_out_list
      ) def_set
    ) block.statements;

  ) cfg.blocks;
  
  (* Una sicurezza per garantire che il registro di output finale del programma sia incluso nell'elenco all_registers, anche se (improbabile) non fosse mai stato usato *)
  add_node output_var;
  
  (* Dereferenzia (!) i nostri ref per restituire i valori finali: la lista di adiacenza completa e l'insieme di tutti i registri usati. *)
  (!graph, !all_registers)

(* Il suo compito è prendere il grafo di interferenza (che dice "chi non può stare con chi") e tentare di fondere i registri virtuali in un numero limitato di registri fisici.
L'obiettivo è assegnare un "colore" (un registro fisico) a ogni "nodo" (registro virtuale) in modo che nessun nodo adiacente (che interferisce) abbia lo stesso colore.
*)
let color_graph (graph : interference_graph) (registers : StringSet.t) : coloring =
  
  (* Inizializza la nostra mappa di "colorazione" (il risultato finale). È un ref perché la costruiremo aggiungendo coppie (registro_virtuale, colore_assegnato) una alla volta. *)
  let coloring = ref [] in
  (* Definisce il nostro "secchiello di colori". È una lista di 25 registri fisici, da "R0" a "R24". *)
  let available_colors = List.init 100 (fun i -> "R" ^ string_of_int i) in 

  (* Itera su ogni singolo registro virtuale (reg) che esiste nel programma (l'elenco registers che abbiamo ottenuto da build_interference_graph). 
  Per ognuno, tenteremo di assegnargli un colore. *)
  StringSet.iter (fun reg ->
    
    (* Cerca la tupla ("R5", ...) nella lista graph. *)
    let neighbors_set = List.assoc_opt reg graph 
    (* Se trova "R5", restituisce il suo set di vicini (es. {"R7", "R9"}). *)
    |> Option.value ~default:StringSet.empty in
    
    (* Ora che sappiamo chi sono i vicini (in neighbors_set), 
    dobbiamo scoprire quali colori hanno già preso. *)
    let used_colors_set =
      (* Itera su ogni neighbor *)
      StringSet.fold (fun neighbor acc_set ->
        (* Cerca quel vicino nella mappa !coloring che stiamo costruendo.*)
        match List.assoc_opt neighbor !coloring with
        (* "Sì, abbiamo già colorato questo vicino (es. "R7") e gli abbiamo assegnato il colore "R1"".
      *)
        | Some color -> StringSet.add color acc_set (* Aggiunge (StringSet.add) quel color ("R1") al nostro used_colors_set*)
        (* "No, non abbiamo ancora colorato questo vicino" *)
        | None -> acc_set (* Non fa nulla *)
      ) neighbors_set StringSet.empty
    in
    
    (* Cerca un colore per il nostro reg. *)
    (* Cerca nella nostra lista available_colors 
    'not (StringSet.mem color used_colors_set)', "Trovami il primo color che NON (not) è presente (mem) nell'elenco dei colori vietati (used_colors_set)"
     *)
    match List.find_opt (fun color -> not (StringSet.mem color used_colors_set)) available_colors with
    (* Successo! Abbiamo trovato un colore (es. "R0") che nessuno dei vicini sta usando.
    Assegniamo quel colore. Aggiungiamo la coppia (reg, color) (es. ("R5", "R0")) alla nostra mappa !coloring. *)
    | Some color -> coloring := (reg, color) :: !coloring
    (* Fallimento! (Questo è il Fallback). I vicini di reg hanno già usato tutti i 25 colori disponibili. Non abbiamo un colore per lui. *)
    | None -> coloring := (reg, reg) :: !coloring (* Non possiamo fonderlo. Rinunciamo all'ottimizzazione per questo registro e gli "assegniamo se stesso" come colore. Aggiungiamo (reg, reg) (es. ("R5", "R5")). Questo significa che R5 rimarrà un registro separato e non sarà fuso. *)
  
  ) registers;
  
  (* Azione: Dopo aver iterato su tutti i registri, dereferenzia (!) la coloring per restituire la lista finale di associazioni, che è la nostra coloring (mappa di colorazione) completa.

Esempio di Output: [ ("R7", "R0"); ("R5", "R0"); ("R9", "R1") ] (Nota: "R5" e "R7" sono stati fusi con successo nel registro fisico "R0").*)
  !coloring

(* Riscrive il CFG usando la mappatura dei registri (colorazione) *)
let rewrite_cfg (cfg : risc_cfg) (coloring : coloring) (output_var : string) : risc_cfg =
  
  (* Questa funzione traduce un registro virtuale nel suo registro fisico/fuso.
  'List.assoc_opt r coloring': Cerca il registro r (es. "R5") nella mappa coloring. 
  '|> Option.value ~default:r:', Se lo trova (es. ("R5", "R0")), restituisce il colore (es. "R0").
      Se non lo trova (come nel caso del Fallback di color_graph, dove r è "R5" e il colore è "R5"), restituisce il nome originale r (es. "R5"). *)
  let find_color r = List.assoc_opt r coloring 
  |> Option.value ~default:r in
  
  (* blocco di sicurezza per gestire il registro di output (output_var, es. "out") 
  let output_color = find_color output_var: Trova il colore assegnato a "out" (es. "R0").

let coloring_with_output = ...: Crea una nuova mappa coloring.

List.remove_assoc output_var coloring: Rimuove qualsiasi mappatura esistente per "out".

(output_var, output_color) :: ...: Aggiunge la mappatura "ufficiale" (es. ("out", "R0")) in testa alla lista.

let find_color r = ...: Ridefinisce find_color per usare questa nuova lista "più sicura" (coloring_with_output). In questo modo, quando List.assoc_opt cercherà "out", troverà subito la mappatura corretta all'inizio della lista.
  *)
  let output_color = find_color output_var in
  let coloring_with_output = (output_var, output_color) :: (List.remove_assoc output_var coloring) in
  let find_color r = List.assoc_opt r coloring_with_output |> Option.value ~default:r in


  (* Per ogni istruzione che usa registri (come Brop), ricostruisce l'istruzione da zero.

Per ogni registro (es. r1, r2, r3), chiama find_color per ottenere il suo nuovo nome (il suo "colore").*)
  let rewrite_instr instr =
    match instr with
    | Brop (op, r1, r2, r3) -> Brop (op, find_color r1, find_color r2, find_color r3)
    | Biop (op, r1, n, r2) -> Biop (op, find_color r1, n, find_color r2)
    | Urop (op, r1, r2) -> Urop (op, find_color r1, find_color r2)
    | Load (r1, r2) -> Load (find_color r1, find_color r2)
    | LoadI (n, r) -> LoadI (n, find_color r)
    | Store (r1, r2) -> Store (find_color r1, find_color r2)
    | CJump (r, l1, l2) -> CJump (find_color r, l1, l2)
    | Nop | Jump _ -> instr
  in

  let new_blocks =
    (* Itera su ogni blocco nel CFG. *)
    List.map (fun (label, block) ->
      (* Applica la funzione rewrite_instr (l'operaio) a ogni istruzione del blocco. new_statements è ora la lista di istruzioni ottimizzate. *)
      let new_statements = List.map rewrite_instr block.statements in
      (* Crea un nuovo blocco che ha la stessa etichetta (label), ma la sua lista di istruzioni (statements) è quella appena riscritta. *)
      (label, { block with statements = new_statements })
    ) cfg.blocks
  in
  (* Alla fine, crea un nuovo record cfg che è identico all'originale, tranne per il fatto che il suo campo blocks è stato sostituito con la nostra lista new_blocks ottimizzata. *)
  { cfg with blocks = new_blocks }

(* Funzione principale di ottimizzazione (questa è la funzione chiamata da Compiler.ml) *)
(* Funzione principale di ottimizzazione (questa è la funzione chiamata da Compiler.ml) *)
let optimize_registers (cfg : risc_cfg) (output_var : string) : risc_cfg =
  Printf.printf "  [Opt] Avvio Liveness Analysis...\n";
  (* 'liveness' è ora una funzione locale a questo modulo *)
  let liveness_table = liveness cfg output_var in
  
  Printf.printf "  [Opt] Costruzione Grafo di Interferenza...\n";
  let graph, registers = build_interference_graph cfg liveness_table output_var in
  
  Printf.printf "  [Opt] Colorazione Registri (Fusione)...\n";
  let coloring = color_graph graph registers in
  
  Printf.printf "  [Opt] Riscittura CFG con registri fusi...\n";
  rewrite_cfg cfg coloring output_var