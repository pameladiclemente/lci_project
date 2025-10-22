(* MiniRISC.ml *)

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

let assign_label () =
  let label = "L" ^ string_of_int !label_counter in
  incr label_counter;
  label

let new_register () =
  let register = "R" ^ string_of_int !register_counter in
  incr register_counter;
  register 

(* Funzione per tradurre un'espressione aritmetica MiniImp in istruzioni MiniRISC *)
let rec aexp_in_risc (a_exp : MiniImp.a_exp) (register : register) : instruction list =
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
let rec bexp_in_risc (b_exp : MiniImp.b_exp) (register : register) : instruction list =
  match b_exp with
  | Boolean true -> [LoadI (1, register)]
  | Boolean false -> [LoadI (0, register)]
  | And (b1, b2) ->
      let r1, r2 = new_register (), new_register () in
      bexp_in_risc b1 r1 @ bexp_in_risc b2 r2 @ [Brop (And, r1, r2, register)]
  | Or (b1, b2) ->
      let r1, r2 = new_register (), new_register () in
      bexp_in_risc b1 r1 @ bexp_in_risc b2 r2 @ [Brop (Or, r1, r2, register)]
  | NotBool b1 -> (* --- CORREZIONE SINTATTICA --- *)
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
let cfgnode_in_risc (node : CFG.node) : labelled_block =
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
let impcfg_in_riscfg (cfg : CFG.cfg) : risc_cfg = 
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


  (* TARGET CODE GENERATION 
(* Controllo che nessun registro sia usato prima di essere definito *)
let check_uninitialized_registers (cfg : risc_cfg) : bool =
  let defined_registers = Hashtbl.create 50 in
  let errors = ref false in
  let check_instr (instr : instruction) =
    let use_regs, def_regs = match instr with
      | Brop (_, r1, r2, r3) -> ([r1; r2], [r3])
      | Biop (_, r1, _, r2) -> ([r1], [r2])
      | Urop (_, r1, r2) -> ([r1], [r2])
      | Load (r1, r2) -> ([r1], [r2]) (* Load usa l'indirizzo in r1 *)
      | LoadI (_, r) -> ([], [r])
      | Store (r1, r2) -> ([r1;r2], []) (* Store usa il valore in r1 e l'indirizzo in r2 *)
      | CJump (r, _, _) -> ([r], [])
      | _ -> ([], [])
    in
    List.iter (fun r ->
      if not (Hashtbl.mem defined_registers r) then (
        Printf.printf "Errore: Il registro %s è usato prima di essere inizializzato!\n" r;
        errors := true
      )
    ) use_regs;
    List.iter (fun r -> Hashtbl.replace defined_registers r true) def_regs
  in
  List.iter (fun (_, block) ->
    List.iter check_instr block.statements
  ) cfg.blocks;
  not !errors











  


(* Traduzione di MiniRISC CFG in MiniRISC con al massimo n registri; 
Converte una singola istruzione MiniRISC in una forma compatibile con n registri.
Se un registro non è tra quelli disponibili in CPU, lo carica dalla memoria temporaneamente.
Se un risultato deve essere scritto in memoria, lo salva dopo l'operazione. 
Se m > n, i registri extra vengono salvati in memoria e caricati solo quando servono!*)
module StringMap = Map.Make(String)

(* Traduzione di MiniRISC CFG in MiniRISC con al massimo n registri *)
(* Traduzione di un'istruzione MiniRISC considerando `n` registri *)
let translate_instruction (memory_map : int StringMap.t) (kept_regs : string list) instr =
  let temp_reg1 = "R_temp1" in
  let temp_reg2 = "R_temp2" in
  let memory_register = "mem" in  (* Registro virtuale per la memoria *)

  (* Mappa i registri: se non è in kept_regs, lo rimpiazziamo con un temporaneo *)
  let map_register r temp_reg =
    if List.mem r kept_regs then r
    else temp_reg
  in

  match instr with
  | Brop (op, r1, r2, r3) ->
      let r1' = map_register r1 temp_reg1 in
      let r2' = map_register r2 temp_reg2 in
      let r3' = map_register r3 temp_reg1 in
      let load_r1 = if not (List.mem r1 kept_regs) then [Load (memory_register, temp_reg1)] else [] in
      let load_r2 = if not (List.mem r2 kept_regs) then [Load (memory_register, temp_reg2)] else [] in
      let store_r3 = if not (List.mem r3 kept_regs) then [Store (temp_reg1, memory_register)] else [] in
      load_r1 @ load_r2 @ [Brop (op, r1', r2', r3')] @ store_r3

  | Biop (op, r1, imm, r3) ->
      let r1' = map_register r1 temp_reg1 in
      let r3' = map_register r3 temp_reg1 in
      let load_r1 = if not (List.mem r1 kept_regs) then [Load (memory_register, temp_reg1)] else [] in
      let store_r3 = if not (List.mem r3 kept_regs) then [Store (temp_reg1, memory_register)] else [] in
      load_r1 @ [Biop (op, r1', imm, r3')] @ store_r3

  | Load (src, dst) ->
      let dst' = map_register dst temp_reg1 in
      let store_dst = if not (List.mem dst kept_regs) then [Store (temp_reg1, memory_register)] else [] in
      [Load (src, dst')] @ store_dst

  | Store (src, dst) ->
      let src' = map_register src temp_reg1 in
      let load_src = if not (List.mem src kept_regs) then [Load (memory_register, temp_reg1)] else [] in
      load_src @ [Store (src', dst)]

  | CJump (r, l1, l2) ->
      let r' = map_register r temp_reg1 in
      let load_r = if not (List.mem r kept_regs) then [Load (memory_register, temp_reg1)] else [] in
      load_r @ [CJump (r', l1, l2)]

  | instr -> [instr]  (* Tutte le altre istruzioni restano invariate *)


(* Traduzione di MiniRISC CFG per un'architettura con `n` registri *)
let translate_to_target (cfg : risc_cfg) (n : int) : risc_cfg =
  if n < 4 then failwith "Il numero di registri deve essere almeno 4";

  let register_usage = Hashtbl.create 50 in

  (* Conta l'uso di ogni registro *)
  List.iter (fun (_, block) ->
    List.iter (function
      | Brop (_, r1, r2, r3) ->
          List.iter (fun r -> Hashtbl.replace register_usage r ((Hashtbl.find_opt register_usage r |> Option.value ~default:0) + 1))
            [r1; r2; r3]
      | Biop (_, r1, _, r3) ->
          List.iter (fun r -> Hashtbl.replace register_usage r ((Hashtbl.find_opt register_usage r |> Option.value ~default:0) + 1))
            [r1; r3]
      | Load (_, r) | LoadI (_, r) | Urop (_, r, _) ->
          Hashtbl.replace register_usage r ((Hashtbl.find_opt register_usage r |> Option.value ~default:0) + 1)
      | Store (r, _) | CJump (r, _, _) ->
          Hashtbl.replace register_usage r ((Hashtbl.find_opt register_usage r |> Option.value ~default:0) + 1)
      | _ -> ()
    ) block.instructions
  ) cfg.blocks;

  (* Seleziona i `n-2` registri più usati *)
  let sorted_regs = 
    Hashtbl.fold (fun r count acc -> (r, count) :: acc) register_usage [] 
    |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1) 
    |> List.map fst
  in
  let kept_regs = List.filteri (fun i _ -> i < n - 2) sorted_regs in
  let stored_regs = List.filteri (fun i _ -> i >= n - 2) sorted_regs in

  (* Mappiamo i registri in memoria con indirizzi interi *)
  let memory_map =
    fst (List.fold_left (fun (acc, addr) r -> 
      (StringMap.add r addr acc, addr + 4))  (* Ogni registro occupa 4 byte *)
      (StringMap.empty, 1000)  (* Iniziamo gli indirizzi da 1000 *)
      stored_regs)
  in

  (* Traduciamo ogni blocco usando `translate_instruction` *)
  let new_blocks = 
    List.map (fun (label, block) -> 
      let new_instrs = List.flatten (List.map (translate_instruction memory_map kept_regs) block.instructions) in
      (label, { block with instructions = new_instrs })
    ) cfg.blocks
  in

  { cfg with blocks = new_blocks }


  (* Conta l'uso dei registri

Per ogni istruzione, incrementa il numero di volte che ogni registro è usato.
2️⃣ Seleziona i n-2 registri più usati

Questi verranno mantenuti in CPU, gli altri verranno salvati in memoria.
3️⃣ Assegna indirizzi di memoria ai registri salvati

Ogni registro in memoria viene mappato a un indirizzo che parte da 1000, avanzando di 4 per ogni registro.
4️⃣ Traduce ogni istruzione

Applica translate_instruction a tutte le istruzioni nel CFG.
5️⃣ Restituisce il CFG modificato

Con le istruzioni tradotte, pronte per essere eseguite su un'architettura con n registri. *)
*)