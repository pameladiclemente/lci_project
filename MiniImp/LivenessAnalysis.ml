open MiniRISC

(* Modulo LivenessAnalysis
 *
 * Implementa il Punto 2 della slide 162: "compute liveness analysis".
 * Questa è una BACKWARD analysis (fluisce dalla fine all'inizio).
 * L'operatore di "merge" (join) è l'UNIONE (∪), perché un registro
 * è vivo se è usato in *almeno uno* dei percorsi futuri (slide 161).
 * Si calcola il LEAST FIXPOINT (LFP) (Slide 196): si parte
 * da ∅ (Bottom, nulla è vivo) e si aggiungono registri man mano
 * che si scoprono i loro usi futuri.
 *)

module StringSet = Set.Make(String)

(* Funzione helper per estrarre registri usati (use) e definiti (def) da una singola istruzione MiniRISC.
 * Questa logica è fondamentale per la formula "lub" della slide 161. *)
 (* '_' è per tutti i valori diversi sa registri, che non ci interessano *)
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

(* Funzione principale per calcolare la Liveness Analysis.
 * Prende il CFG di MiniRISC e il nome del registro di output.
 * Restituisce una Hashtbl che mappa ogni etichetta (label)
 * a una coppia (live_in, live_out) di set di registri. *)
let liveness (cfg : MiniRISC.risc_cfg) (output : string) : (MiniRISC.label, StringSet.t * StringSet.t) Hashtbl.t =
  
  let liveness_table = Hashtbl.create (List.length cfg.blocks) in

  (* 1. INIZIALIZZAZIONE (Least Fixpoint: partiamo da ∅ - Bottom) *)
  List.iter (fun (label, _) ->
    (* Inizializziamo live_in e live_out di tutti i blocchi a ∅ *)
    Hashtbl.add liveness_table label (StringSet.empty, StringSet.empty)
  ) cfg.blocks;

  (* REQUISITO SLIDE 161/162: "mind... out which is always used"
   * Forziamo il registro di output ad essere "vivo"
   * all'ingresso del blocco terminale. Questo agisce da "seme"
   * per l'analisi backward. *)
  let terminal_live_in = StringSet.singleton output in
  let _, terminal_live_out = Hashtbl.find liveness_table cfg.terminal_node in
  Hashtbl.replace liveness_table cfg.terminal_node (terminal_live_in, terminal_live_out);


  (* 2. CALCOLO DEL PUNTO FISSO (Iterativo) *)
  let changed = ref true in
  while !changed do
    changed := false;
    
    (* Iteriamo su tutti i blocchi *)
    List.iter (fun (label, block) ->
      
      let old_live_in, old_live_out = Hashtbl.find liveness_table label in

      (* 2a. Calcola new_live_out(L) = ∪(S ∈ succ(L)) live_in(S) *)
      (* (Formula "lucf" slide 161, il merge è l'UNIONE) *)
      let new_live_out =
        List.fold_left (fun computed_live_registers successor_label ->
          let succesor_live_in, _ = Hashtbl.find liveness_table successor_label in
          StringSet.union computed_live_registers succesor_live_in
        ) StringSet.empty block.edges 
      in
      (* Alla fine del 'fold', 'new_live_out' conterrà l'unione dei set 'live_in'
      di *tutti* i successori in 'block.edges', che è esattamente
      l'implementazione della formula della slide 161. *)

      (* 2b. Calcola use(L) e def(L) del blocco
       * Scorre le istruzioni ALL'INDIETRO (List.fold_right)
       * per propagare la "vità" (liveness) verso l'alto. *)
      let block_use_reg, block_def_reg = 
        List.fold_right (fun instruction (computed_use_reg, computed_def_reg) ->
          let instruction_use_reg, instruction_def_reg = use_def_registers instruction in
          (* Formula (slide 161):
           * use(B) = use(i) ∪ (use(B_dopo) - def(i))
           * def(B) = def(i) ∪ def(B_dopo)
           *)
          let new_use_reg = StringSet.union instruction_use_reg (StringSet.diff computed_use_reg instruction_def_reg) in
          let new_def_reg = StringSet.union instruction_def_reg computed_def_reg in
          (new_use_reg, new_def_reg)
        ) block.statements (StringSet.empty, StringSet.empty) 
      in

      (* 2c. Calcola new_live_in(L) = use(L) ∪ (live_out(L) - def(L)) *)
      (* (Formula "lub" slide 161) *)
      let new_live_in = 
        StringSet.union block_use_reg (StringSet.diff new_live_out block_def_reg)
      in
      
      (* Seme: il live_in del nodo terminale è sempre {out} *)
      let final_new_live_in = 
        if label = cfg.terminal_node then
          terminal_live_in
        else
          new_live_in
      in

      (* 3. CONTROLLO STABILITÀ *)
      if not (StringSet.equal old_live_in final_new_live_in && 
              StringSet.equal old_live_out new_live_out) 
      then (
        Hashtbl.replace liveness_table label (final_new_live_in, new_live_out);
        changed := true
      )
    ) cfg.blocks;
  done;

  (* Restituisce la tabella completa con i risultati finali *)
  liveness_table

