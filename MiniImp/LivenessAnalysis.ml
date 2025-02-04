module LivenessAnalysis = struct

  (* Creiamo un Set di stringhe per rappresentare i registri vivi; 
  usiamo StringSet perchè vogliamo solo sapere quali registri/variabili sono vivi in un dato punto del programma *)
  module StringSet = Set.Make(String)

  (* Funzione principale per la Liveness Analysis;
  prende in input un risc_cfg e restituisce una mappa (Hashtbl) che associa a ogni blocco:
  live_in: registri vivi prima del blocco.
  live_out: registri vivi dopo il blocco.*)
  let compute_liveness (cfg : MiniRISC.risc_cfg) : (MiniRISC.label, StringSet.t * StringSet.t) Hashtbl.t =
    
    (* 
    Creiamo una tabella hash (Hashtbl) per memorizzare live_in e live_out di ogni blocco.
    50 è un valore arbitrario scelto per la capacità iniziale della tabella. 
    Usiamo una Hashtbl perchè rispetto a StringMap è più veloce O(1) e perchè StrigMap.Add
    ogni volta restituisce una nuova tabella, meno efficiente *)
    let liveness_table = Hashtbl.create 50 in

    (* 
    Riferimento: Slide 15 ("Analysis State")

    Ogni blocco in cfg.blocks inizia con live_in = ∅ (= insieme di variabili vive all'inizio di un blocco (cioè usate prima di essere ridefinite)) 
    e live_out = ∅ (= insieme di variabili vive alla fine di un blocco (cioè ancora necessarie nei blocchi successivi).).
    Usiamo List.iter per iterare su tutti i blocchi e inizializzarli nella Hashtbl.
    Per ogni blocco (block), inserisce in liveness_table:
    - live_in = ∅ -> StringSet.empty (nessuna variabile è viva all'inizio).
    - live_out = ∅ -> StringSet.empty (nessuna variabile è viva alla fine)
        
    Inizializziamo live_in e live_out per ogni blocco *)
    List.iter (fun (labelled_block, _) ->
      Hashtbl.add liveness_table labelled_block (StringSet.empty, StringSet.empty)
    ) cfg.MiniRISC.blocks;

    (* Riferimento: Slide 15 ("Global Update applies all the local updates as before.")

    Algoritmo iterativo: ripetiamo il calcolo finché live_in e live_out non cambiano più, per cui avviene la
    dichiarazione della variabile changed, che tiene traccia se ci sono state modifiche in questa iterazione.
    Ripetiamo il calcolo finché i valori non smettono di cambiare *)

    let do_update = ref true in
    while !do_update do

      (* Impostiamo do_update a false:
      Se, alla fine dell'iterazione, live_in o live_out cambiano, lo reimposteremo a true per ripetere il ciclo.
      Se do_update rimane false, significa che abbiamo raggiunto il punto fisso e possiamo terminare.*)
      do_update := false;

      
      (*  Riferimento: Slide 15 ("Analysis State")
      Iteriamo su tutti i blocchi del CFG.
      - labelled_block è l'etichetta del blocco (es. "L1", "L2").
      - block è la struttura del blocco, che contiene:
          - instructions: la lista di istruzioni del blocco.
          - successors: la lista di blocchi successori. *)
      List.iter (fun (labelled_block, block) ->

        (* Recuperiamo i vecchi valori di live_in e live_out per il blocco corrente:
        Questo ci permette di confrontarli dopo l'aggiornamento; Se i valori rimangono gli stessi, l'algoritmo terminerà presto *)
        let old_live_in, old_live_out = Hashtbl.find liveness_table labelled_block in

        (* 
        Riferimento: Slide 15 ("Local Update - From a block to the others")

        Calcoliamo il nuovo live_out per il blocco attuale:
        1. Per ogni successore succ_block, prendiamo il suo live_in.
        2. Facciamo l'unione (union) di tutti i live_in dei successori (perchè così è la regola: 
        live_out di un blocco è l'unione (⋃) di live_in dei suoi successori).
        3. Se il blocco ha più successori, live_out sarà l'unione di tutti i loro live_in. *)
        let new_live_out =
          List.fold_left (fun acc succ_block ->
            let succ_live_in, _ = Hashtbl.find liveness_table succ_block in
            StringSet.union acc succ_live_in
          ) StringSet.empty block.MiniRISC.successors
        in

        (* 
        Riferimento: Slide 15 ("Local Update - From the block itself")

        Dobbiamo determinare used_registers e defined_registers, ovvero:
        - used_registers: variabili usate nel blocco, che quindi devono essere già vive.
        - defined_registers: variabili definite nel blocco, che quindi sovrascrivono il valore precedente.
        
        ! fold_left scorre tutte le istruzioni nel blocco e accumula used_registers e defined_registers.
        
        Usiamo fold_left per accumulare tutti i registri usati e definiti nel blocco.
                
        live_in = (registri usati in questo blocco) ∪ (live_out - registri definiti) *)
        let used_registers, defined_registers =

        (* fold_left è una funzione di riduzione che scorre una lista accumulando un valore intermedio; L'accumulatore acc è il valore intermedio che si aggiorna a ogni iterazione di fold_left..
        Calcolare live_out di un blocco come l'unione (∪) di live_in di tutti i suoi successori.
        Passaggi nel codice
          1. Valore iniziale → StringSet.empty (insieme vuoto).
          2. Iteriamo sui successori (block.MiniRISC.successors).
          3. Per ogni successore succ_block:
              - Prendiamo il suo live_in dalla tabella liveness_table.
              - Facciamo l'unione (union) con acc (valore accumulato).
          4. Alla fine, acc contiene live_out, che è l'unione di tutti i live_in dei successori.
          ! Conclusione: Questo garantisce che se un registro è live all'inizio di un successore, deve essere live alla fine del blocco corrente.


        Partiamo con due insiemi vuoti: used_registers = ∅, defined_registers = ∅.
        uses e defs sono l'accumulatore, cioè gli insiemi aggiornati ad ogni iterazione. 
        Identifichiamo used_registers e defined_registers per l'istruzione corrente *)
        
          List.fold_left (fun (uses, defs) instr ->


            (* Identifichiamo used_registers e defined_registers per ogni tipo di istruzione:
            Brop (op, r1, r2, r3): usa r1, r2, definisce r3.
            Load (_, r): definisce r, ma non usa nessun registro.
            Store (r1, _): usa r1, ma non definisce nulla.
            CJump (r, _, _): usa r per la condizione di salto.
            
            
            Perché non consideriamo Nop in use_regs e def_regs? Nop (No Operation) è un'istruzione speciale che non fa nulla.
            Nel contesto della Liveness Analysis, ha due caratteristiche chiave:
            Non usa registri (use_regs = []) perché non legge nessun valore.
            Non definisce registri (def_regs = []) perché non scrive nulla.*)
            let u, d = match instr with
              | MiniRISC.Brop (_, r1, r2, r3) -> ([r1; r2], [r3])
              | MiniRISC.Biop (_, r1, _, r2) -> ([r1], [r2])
              | MiniRISC.Urop (_, r1, r2) -> ([r1], [r2])
              | MiniRISC.Load (_, r) -> ([], [r])
              | MiniRISC.LoadI (_, r) -> ([], [r])
              | MiniRISC.Store (r1, _) -> ([r1], [])
              | MiniRISC.CJump (r, _, _) -> ([r], [])
              | _ -> ([], [])
            in

            (* Accumula used_registers e defined_registers per tutte le istruzioni del blocco:
            1. Prendiamo i registri usati e definiti dall'istruzione corrente.
            2. Aggiungiamo questi registri ai Set uses e defs; uses ∪ {u}, defs ∪ {d}.
            Alla fine, used_registers e defined_registers conterranno tutti i registri usati e definiti nel blocco.*)
            (StringSet.union uses (StringSet.of_list u),
             StringSet.union defs (StringSet.of_list d))
          ) (StringSet.empty, StringSet.empty) block.MiniRISC.instructions
        in

        (* 
        Riferimento: Slide 15
        Per la regola di liveness ("lub(lvin(L)) = {r used in L} ∪ (lvout(L) \ {r defined in L})"),
        le variabili usate (used_registers) sono sempre live_in;
        Le variabili definite (defined_registers) non sono più necessarie perché vengono sovrascritte.
        Le variabili in live_out che non vengono ridefinite rimangono live_in. *)
        let new_live_in = StringSet.union used_registers (StringSet.diff new_live_out defined_registers) in

        (*
        
        Riferimento: Slide 15 ("Global Update applies all the local updates as before.")

        Se live_in o live_out sono cambiati, aggiorniamo la Hashtbl.
        Se c'è un cambiamento, ripetiamo il calcolo (do_update := true).*)
        if not (StringSet.equal old_live_in new_live_in && StringSet.equal old_live_out new_live_out) then (
          Hashtbl.replace liveness_table labelled_block (new_live_in, new_live_out);
          do_update := true
        )
        (* Riferimento: Slide 15 ("Global Update applies all the local updates as before.")

        Ripetiamo finché raggiungiamo un punto fisso.
        Restituiamo la tabella con i risultati. *)
      ) cfg.MiniRISC.blocks;
    done;

    liveness_table
end


