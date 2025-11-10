(* MiniRISC.ml *)
open MiniImp
open CFG
open LivenessAnalysis
module StringSet = LivenessAnalysis.StringSet 
(* Remove local StringSet definition and use LivenessAnalysis.StringSet everywhere *)

(* Identificatori per registri e etichette *)
type register = string
type label = string

(* Operazioni binarie, binarie immediate e unarie *)

(* tra i valori contenuti in due registri sorgente e scrivono il risultato in un registro di destinazione 
(es. add r1 r2 => r3).*)
type brop = 
  | Add | Sub | Mul | Div | Mod | And | Or
  | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | Equal
             
(* tra un registro e un valore costante (detto "immediato") e scrivono il risultato in un registro 
(es. addI r1 5 => r2). *)
type biop = 
  | AddInt | SubInt | MultInt | DivInt | ModInt | AndInt | OrInt   
    
(* Operano su un singolo registro e scrivono il risultato in un altro 
(es. not r1 => r2). *)
type urop = 
  | Not | Copy                    

(* Istruzioni di MiniRISC *)
type instruction =
  | Nop
  | Brop of brop * register * register * register
  | Biop of biop * register * int * register
  | Urop of urop * register * register
  | Load of register * register (* Interagiscono con la memoria principale (RAM). *)
  | LoadI of int * register (* Load Immediate: Carica un valore costante direttamente in un registro. *)
  | Store of register * register
  | Jump of label (* Jump è un salto incondizionato a un'etichetta *)
  | CJump of register * label * label (* CJump salta a una delle due etichette a seconda del valore *)

(* Blocchi etichettati (liste di istruzioni) *)
type labelled_block = {
  label: label;
  statements: instruction list;
  edges: label list;
}

(* Grafo di controllo di flusso per MiniRISC , Rappresentazione Intermedia (IR) *)
type risc_cfg = {
  blocks: (label * labelled_block) list;
  entry_node: label;
  terminal_node: label;
}

(* Contatori per generare etichette e registri unici *)
let label_counter = ref 0
let register_counter = ref 0

let new_label () =
  let label = "L" ^ string_of_int !label_counter in
  incr label_counter;
  label

let new_register () =
  let register = "R" ^ string_of_int !register_counter in
  incr register_counter;
  register 

(* Funzione per tradurre un'espressione aritmetica MiniImp in istruzioni MiniRISC *)
let rec aexp_in_risc (a_exp : a_exp) (register : register) : instruction list =
  match a_exp with
  | Integer n -> [LoadI (n, register)] 
  | Variable x -> [Load (x, register)] (* Semplificazione: trattiamo 'x' come indirizzo/registro *)
  | Add (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (Add, r1, r2, register)]
  | Sub (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (Sub, r1, r2, register)]
  | Mul (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (Mul, r1, r2, register)]
  | Div (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (Div, r1, r2, register)]
  | Mod (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (Mod, r1, r2, register)]
  | NotInt a1 -> (* --- ERRORE CORRETTO --- *)
      let r1 = new_register() in
      let zero_reg = new_register() in
      aexp_in_risc a1 r1 @ [LoadI(0, zero_reg)] @ [Brop (Sub, zero_reg, r1, register)]

(* Funzione per tradurre un'espressione booleana MiniImp in istruzioni MiniRISC *)
let rec bexp_in_risc (b_exp : b_exp) (register : register) : instruction list =
  match b_exp with
  | Boolean true -> [LoadI (1, register)]
  | Boolean false -> [LoadI (0, register)]
  | And (b1, b2) ->
      let r1, r2 = new_register (), new_register () in
      bexp_in_risc b1 r1 @ bexp_in_risc b2 r2 @ [Brop (And, r1, r2, register)]
  | Or (b1, b2) ->
      let r1, r2 = new_register (), new_register () in
      bexp_in_risc b1 r1 @ bexp_in_risc b2 r2 @ [Brop (Or, r1, r2, register)]
  | NotBool b1 -> 
      let r1 = new_register () in
      bexp_in_risc b1 r1 @ [Urop (Not, r1, register)]
  | LessThan (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (LessThan, r1, r2, register)]
  | LessThanEqual (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (LessThanEqual, r1, r2, register)]
  | GreaterThan (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (GreaterThan, r1, r2, register)]
  | GreaterThanEqual (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (GreaterThanEqual, r1, r2, register)]
  | Equal (a1, a2) ->
      let r1, r2 = new_register (), new_register () in
      aexp_in_risc a1 r1 @ aexp_in_risc a2 r2 @ [Brop (Equal, r1, r2, register)]
 
(* Funzione per tradurre un nodo CFG MiniImp in un blocco MiniRISC *)
let cfgnode_in_risc (node : node) : labelled_block =
  let label = "L" ^ string_of_int node.id in (* Node with id = 5 is now label L5 *)
  let base_instrs =
    List.flatten (List.map (function
      | MiniImp.Skip -> [Nop]
      | MiniImp.Assign (x, a_exp) ->
          let register = new_register () in
          aexp_in_risc a_exp register @ [Store (register, x)]
      | MiniImp.If (cond, _, _) ->
          let register = new_register () in
          bexp_in_risc cond register @
          [CJump (register, "L" ^ string_of_int (List.nth node.edges 0), "L" ^ string_of_int (List.nth node.edges 1))]
      | MiniImp.While (cond, _) ->
          let register = new_register () in
          bexp_in_risc cond register @
          [CJump (register, "L" ^ string_of_int (List.nth node.edges 0), "L" ^ string_of_int (List.nth node.edges 1))]
      | _ -> [] 
      (* Seq non c'è 
      Un comando come c1; c2 (Seq(c1, c2)) non viene messo dentro un singolo nodo. 
      Al contrario, viene rappresentato come una connessione (un arco) tra nodi *)
    ) node.statements)
  in
  (* --- MIGLIORAMENTO LOGICO --- *)
  (* Se un blocco non termina con un salto condizionale (CJump) e ha un solo successore, 
     deve terminare con un salto incondizionato (Jump) per mantenere il flusso corretto. *)
  let final_instrs =
    if base_instrs <> [] && List.length node.edges = 1 then
      match List.hd (List.rev base_instrs) with
      | CJump _ -> base_instrs
      | _ -> base_instrs @ [Jump ("L" ^ string_of_int (List.hd node.edges))]
    else
      base_instrs
  in
  { label = label; statements = final_instrs; edges = List.map (fun id -> "L" ^ string_of_int id) node.edges }


(* Traduzione di un intero CFG di MiniImp a MiniRISC CFG , prima trans_in_riscfg *)
let impcfg_in_riscfg (cfg : cfg) : risc_cfg = 
  let blocks = List.map (fun (_, node) -> let block = cfgnode_in_risc node in (block.label, block)) cfg.nodes in
  { blocks; entry_node = "L" ^ string_of_int cfg.entry_node; terminal_node = "L" ^ string_of_int cfg.terminal_node }


(* Funzione per tradurre una singola istruzione in stringa Converte i nostri tipi di dati OCaml (es. Add) nella loro sintassi testuale (es. "add")*)
let instruction_to_string = function
  | Nop -> "nop"
  | Brop (op, r1, r2, r3) -> Printf.sprintf "%s %s %s => %s"
      (match op with
        | Add -> "add" 
        | Sub -> "sub" 
        | Mul -> "mult" 
        | Div -> "div" 
        | Mod -> "mod"
        | And -> "and" 
        | Or -> "or" 
        | LessThan -> "less" 
        | LessThanEqual -> "less_eq"
        | GreaterThan -> "greater" 
        | GreaterThanEqual -> "greater_eq" 
        | Equal -> "equal"
      ) r1 r2 r3
  | Biop (op, r1, n, r2) -> Printf.sprintf "%s %s %d => %s"
      (match op with
        | AddInt -> "addI" 
        | SubInt -> "subI" 
        | MultInt -> "multI"
        | DivInt -> "divI" 
        | ModInt -> "modI" 
        | AndInt -> "andI" 
        | OrInt -> "orI"
      ) r1 n r2
  | Urop (op, r1, r2) -> Printf.sprintf "%s %s => %s"
      (match op with 
      | Not -> "not" 
      | Copy -> "copy"
      ) r1 r2
  | Load (r1, r2) -> Printf.sprintf "load %s => %s" r1 r2
  | LoadI (n, r) -> Printf.sprintf "loadI %d => %s" n r
  | Store (r1, r2) -> Printf.sprintf "store %s => %s" r1 r2
  | Jump label -> Printf.sprintf "jump %s" label
  | CJump (r, l1, l2) -> Printf.sprintf "cjump %s %s %s" r l1 l2

(* --- FUNZIONE AGGIUNTA --- *)
(* Funzione per tradurre l'intero CFG MiniRISC in codice Assembly MiniRISC *)
let riscfg_in_assembly (cfg : risc_cfg) : string =
  let buffer = Buffer.create 1024 in
  List.iter (fun (label, block) ->
    Buffer.add_string buffer (label ^ ":\n");
    List.iter (fun instr ->
      Buffer.add_string buffer ("  " ^ instruction_to_string instr ^ "\n")
    ) block.statements;
  ) cfg.blocks;
  Buffer.contents buffer


(* Target Code Generation *)




(* Traduzione di MiniRISC CFG in MiniRISC con al massimo n registri; 
Converte una singola istruzione MiniRISC in una forma compatibile con n registri.
Se un registro non è tra quelli disponibili in CPU, lo carica dalla memoria temporaneamente.
Se un risultato deve essere scritto in memoria, lo salva dopo l'operazione. 
Se m > n, i registri extra vengono salvati in memoria e caricati solo quando servono!*)



(* Funzione helper per 'translate_instruction'. 
   Genera il codice per caricare un registro "spillato" (dalla memoria). *)

(* Questa funzione viene chiamata quando il compilatore deve generare codice per un'istruzione (es. add r5 r6 => r7) 
   e scopre che uno dei registri sorgente (es. r5) non è in un registro fisico, ma è stato "riversato" (spilled) in memoria.
  L'obiettivo della funzione è generare la lista di istruzioni MiniRISC per recuperare quel valore dalla memoria 
  e metterlo in un registro fisico temporaneo in modo che la CPU possa usarlo.  *)
let load_spilled (register : register) (register_map : int StringMap.t) (temp_register : register) : instruction list =
  (* Cerca l'indirizzo di memoria di 'r' *)
  let address = StringMap.find register register_map in
  [
    LoadI (address, temp_register);  (* 1. Carica l'indirizzo nel registro temporaneo *)
    Load (temp_register, temp_register)  (* 2. Carica il valore da quell'indirizzo nel registro temporaneo *)
    (* cioè:
    Guarda cosa c'è in R_temp1 (c'è il numero 1000). Tratta quel numero come un indirizzo. 
    Vai a quell'indirizzo nella RAM (allo scomparto 1000), prendi il valore che c'è lì 
    (es. il numero 42, che era il nostro valore di R5), 
    e mettilo di nuovo in R_temp1, sovrascrivendo quello che c'era prima.*)
  ]

(* Funzione helper per 'translate_instruction'.
   Genera il codice per salvare un valore in un registro "spillato" (in memoria). *)
let store_spilled (register : register) (register_map : int StringMap.t) (res_register : register) (temp_addr_register : register) : instruction list =
  (* Cerca l'indirizzo di memoria di 'register' *)
  let addr = StringMap.find register register_map in
  [
    LoadI (addr, temp_addr_register); (* 1. Carica l'indirizzo nel registro temporaneo per l'indirizzo *)
    Store (res_register, temp_addr_register) (* 2. Salva il valore (da res_register) in quell'indirizzo *)
  ]

(* Traduzione di un'istruzione MiniRISC considerando `n` registri (register spilling) 
Il suo compito è prendere una singola istruzione MiniRISC che è stata scritta assumendo di avere infiniti registri (es. "R1", "R5", "R7", "R20"...) 
e tradurla in una lista di istruzioni MiniRISC che funziona su una macchina che ha solo n registri fisici (es. n=4).*)
let translate_instruction (register_map : int StringMap.t) (kept_registers : string list) 
                          (temp_register1 : register) (temp_register2 : register) (instruction : instruction) : instruction list =

  (* Funzione helper per mappare un registro virtuale a uno fisico (o a un temporaneo) *)
  let map_register register temp_register =
    if List.mem register kept_registers then register (* È un registro fisico, usa se stesso *)
    else temp_register (* È un registro "spillato", usa il temporaneo al suo posto *)
  in

  match instruction with
  (* add r1, r2 => r3 *)
  | Brop (op, r1, r2, r3) ->
      let r1_reg = map_register r1 temp_register1 in
      let r2_reg = map_register r2 temp_register2 in
      let r3_reg = map_register r3 temp_register1 in (* Possiamo riusare temp_register1 per il risultato *)
      
      (* 1. Carica i registri sorgente se sono spillati, cioè non in kept_registers *)
      let load_r1 = if not (List.mem r1 kept_registers) then load_spilled r1 register_map temp_register1 else [] in
      let load_r2 = if not (List.mem r2 kept_registers) then load_spilled r2 register_map temp_register2 else [] in
      
      (* 2. Salva il risultato se il registro di destinazione è spillato *)
      let store_r3 = if not (List.mem r3 kept_registers) then store_spilled r3 register_map r3_reg temp_register2 else [] in
      
      (* Codice finale: Carica -> Carica -> Operazione -> Salva *)
      load_r1 @ load_r2 @ [Brop (op, r1_reg, r2_reg, r3_reg)] @ store_r3

  (* addI r1, 10 => r3 *)
  | Biop (op, r1, imm, r3) ->
      let r1_reg = map_register r1 temp_register1 in
      let r3_reg = map_register r3 temp_register1 in
      
      let load_r1 = if not (List.mem r1 kept_registers) then load_spilled r1 register_map temp_register1 else [] in
      let store_r3 = if not (List.mem r3 kept_registers) then store_spilled r3 register_map r3_reg temp_register2 else [] in
      
      load_r1 @ [Biop (op, r1_reg, imm, r3_reg)] @ store_r3

  (* not r1 => r2 *)
  | Urop (op, r1, r2) ->
      let r1_reg = map_register r1 temp_register1 in
      let r2_reg = map_register r2 temp_register1 in
      
      let load_r1 = if not (List.mem r1 kept_registers) then load_spilled r1 register_map temp_register1 else [] in
      let store_r2 = if not (List.mem r2 kept_registers) then store_spilled r2 register_map r2_reg temp_register2 else [] in
      
      load_r1 @ [Urop (op, r1_reg, r2_reg)] @ store_r2

  (* load r1 => r2 *)
  (* supponiamo che il valore di r5 che è un indirizzo, p.e. 3000, è in cella memoria 1000, quindi in register_map abbiamo (r1->1000) *)
  | Load (r_addr, r_dest) -> (* sono entrambi registri virtuali; l'istruzione significa 
  Prendi l'indirizzo che si trova nel registro r_addr, vai a quell'indirizzo nella RAM, 
  prendi il valore che c'è lì, e mettilo nel registro r_dest*)

(* Decide quale registro fisico conterrà l'indirizzo da cui leggere, 
cioè r_addr ma in un indirizzo fisico invece che virtuale r1. *)
      let r_addr_reg = map_register r_addr temp_register1 in 
      (* r_addr_reg diventerà r_temp1,Useremo R_temp1 per contenere l'indirizzo 
      (3000) che prenderemo da valore di r1 cioè 1000 *)
      let r_dest_reg = map_register r_dest temp_register2 in
      
      (* Genera codice per andare all'indirizzo 1000, prendere il valore che c'è lì (che è 3000) e metterlo in R_temp1. *)
      let load_addr = if not (List.mem r_addr kept_registers) then load_spilled r_addr register_map temp_register1 else [] in
      let store_dest = if not (List.mem r_dest kept_registers) then store_spilled r_dest register_map r_dest_reg temp_register1 else [] in
      
      load_addr @ [Load (r_addr_reg, r_dest_reg)] @ store_dest

  (* loadI 42 => r1 *)
  | LoadI (imm, r_dest) ->
      let r_dest_reg = map_register r_dest temp_register1 in
      let store_dest = if not (List.mem r_dest kept_registers) then store_spilled r_dest register_map r_dest_reg temp_register2 else [] in
      
      [LoadI (imm, r_dest_reg)] @ store_dest

  (* store r1 => r2 *)
  | Store (r_src, r_addr) ->
      
      let r_src_reg = map_register r_src temp_register1 in
      let r_addr_reg = map_register r_addr temp_register2 in
      
      let load_src = if not (List.mem r_src kept_registers) then load_spilled r_src register_map temp_register1 else [] in
      let load_addr = if not (List.mem r_addr kept_registers) then load_spilled r_addr register_map temp_register2 else [] in
      
      load_src @ load_addr @ [Store (r_src_reg, r_addr_reg)]

  (* cjump r5, l1, l2 *)
  | CJump (r, l1, l2) ->
      let r_reg = map_register r temp_register1 in
      let load_r = if not (List.mem r kept_registers) then load_spilled r register_map temp_register1 else [] in
      
      load_r @ [CJump (r_reg, l1, l2)]

  | Nop | Jump _ -> [instruction]  (* Nop e Jump restano invariati *)


(* Traduzione di MiniRISC CFG per un'architettura con `n` registri *)
let translate_to_target (cfg : risc_cfg) (n : int) : risc_cfg =
  if n < 4 then failwith "Il numero di registri (n) deve essere almeno 4 (2 per Brop, 2 per spilling)";
  let r_temp1 = "R_temp1" in
  let r_temp2 = "R_temp2" in

  let counting_table = Hashtbl.create 50 in

  (* Funzione helper per contare l'uso, ignorando i registri temporanei 
  Questa è un'ottima domanda. Questo blocco di codice è il cervello strategico della funzione translate_to_target.
  Il suo scopo è implementare una regola intelligente per decidere quali registri 
  vale la pena tenere nei registri fisici della CPU e quali spillare nella memoria RAM.
  
  Questo blocco di codice è il contatore. Il suo unico scopo è scorrere il programma, 
  istruzione per istruzione e contare quante volte viene usato ogni registro virtuale.
  
  'counting_table'  ci dirà:
    R1: usato 15 volte
    R5: usato 2 volte
    R7: usato 9 volte

  *)
  let update_count register =
    (* non ci interessano i registri temporanei *)
    if register <> r_temp1 && register <> r_temp2 then
      (*
      'Hashtbl.find_opt counting_table r': "Guarda nella tabella dei punteggi (counting_table) se c'è già un punteggio per r."
      '|> Option.value ~default:0': "Se find_opt restituisce None (cioè r non è ancora nella tabella), usa 0 come punteggio. Altrimenti, usa il punteggio che hai trovato (es. 7)."
      *)
      let count = (Hashtbl.find_opt counting_table register |> Option.value ~default:0) + 1 in
      (* "Aggiorna la tabella dei punteggi. Il punteggio di r è ora questo nuovo valore." *)
      Hashtbl.replace counting_table register count
  in

  (*Visto che istruzioni come Brop usano 3 registri, questa funzione ci permette di chiamare add_usage su ogni elemento di una lista (es. [r1; r2; r3]) con una sola chiamata. *)
  let update_count_list l = List.iter update_count l in

  (* 1. Conta l'uso di ogni registro *)
  List.iter (fun (_, block) -> (* "Per ogni blocco nel CFG..." *)
    List.iter (fun instr -> (* "...per ogni istruzione in quel blocco..." *)
      match instr with
      | Brop (_, r1, r2, r3) -> update_count_list [r1; r2; r3]
      | Biop (_, r1, _, r3) -> update_count_list [r1; r3]
      | Urop (_, r1, r2) -> update_count_list [r1; r2]
      | Load (r1, r2) -> update_count_list [r1; r2]
      | LoadI (_, r) -> update_count r
      | Store (r1, r2) -> update_count_list [r1; r2]
      | CJump (r, _, _) -> update_count r
      | _ -> ()
    ) block.statements 
  ) cfg.blocks;

  (* 2. Seleziona i `n-2` registri "generali" più usati *)
  let sort_registers = 
    (* Converte la tabella hash register_usage (la nostra "tabella punteggi") in una semplice lista di coppie (registro, punteggio). *)
    Hashtbl.fold (fun r count acc -> (r, count) :: acc) counting_table [] 
    (* Invia (|>) la lista alla funzione di ordinamento. La funzione compare c2 c1 ordina in ordine decrescente (dal punteggio più alto al più basso). *)
    |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1) 
    (* Invia la lista ordinata a List.map fst. fst prende solo il primo elemento di ogni coppia (il nome del registro), scartando il punteggio. *)
    |> List.map fst
  in
  
  (* I registri fisici che useremo sono i nostri 2 temporanei + i top (n-2) *)
  let sort_kept_registers = List.filteri (fun i _ -> i < (n - 2)) sort_registers in (* i registri che prendiamo da counting_table *)
  let kept_registers = r_temp1 :: r_temp2 :: sort_kept_registers in (*Costruisce la lista finale di tutti i registri che saranno fisicamente nella CPU.*)

  (* i registri scartati da counting_table diventano spillati, quindi *)
  (* filtriamo la tabella  *)
  let stored_regs = List.filter (fun r -> not (List.mem r kept_registers)) sort_registers in

  (* 3. Assegna indirizzi di memoria ai registri "spillati" *)
  let memory_map =
    (* tera (List.fold_left) sulla lista stored_regs (["R5"]) per costruire una StringMap. *)
    fst (List.fold_left (fun (acc, addr) r -> 
      (StringMap.add r addr acc, addr + 4))  (* Ogni registro "costa" 4 byte *)
      (StringMap.empty, 1000)  (* Iniziamo con una mappa vuota (acc) e decidiamo arbitrariamente di iniziare a salvare le cose all'indirizzo di memoria 1000 (addr). *)
      stored_regs)
  in

  (* 4. Traduciamo ogni blocco usando `translate_instruction` *)
  let new_blocks = 
    List.map (fun (label, block) -> (* Itera su ogni blocco del CFG originale.*)
      (*
      'translate_instruction memory_map kept_regs ...': Questo "congela" i nostri strumenti. È una funzione che aspetta solo un'istruzione (instr) per tradurla.
      'List.map (translate_instruction ...) block.statements': Esegue la traduzione su ogni istruzione nel blocco. Poiché translate_instruction restituisce una lista di istruzioni (es. [LoadI; Load; Brop; Store]), il risultato di List.map è una lista di liste:
          [ [LoadI; Load; Brop; Store]; [LoadI; Store]; [CJump] ]
      'List.flatten (...)': Appiattisce la lista di liste in una singola lista di istruzioni.
          [ LoadI; Load; Brop; Store; LoadI; Store; CJump ]
    
      *)
      let new_instrs = List.flatten (List.map (translate_instruction memory_map kept_registers r_temp1 r_temp2) block.statements) in
      (* --- CORREZIONE: da 'block.instructions' a 'block.statements' --- *)

      (*Crea un nuovo blocco con la stessa etichetta, ma con il campo statements sostituito dalla nostra nuova lista di istruzioni fisiche.*)
      (label, { block with statements = new_instrs })
    ) cfg.blocks
  in

  (* 5. Restituisci il nuovo CFG tradotto *)
  { cfg with blocks = new_blocks }




  