(* MiniRISC.ml *)

(* Identificatori per registri e etichette *)
type register = string
type label = string

(* Operazioni binarie, binarie immediate e unarie *)
type brop = 
  | Add               (* add *)
  | Sub               (* sub *)
  | Mul               (* mult *)
  | Div               
  | Mod 
  | And               (* and *)
  | Or
  | LessThan          (* less *)
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Equal
             
type biop = 
  | AddInt            (* addI *)
  | SubInt            (* subI *)
  | MultInt           (* multI *)
  | DivInt  
  | ModInt  
  | AndInt            (* andI *)  
  | OrInt   
    
type urop = 
  | Not               (* not *)
  | Copy              (* copy *)                    

(* Istruzioni di MiniRISC *)
type instruction =
  | Nop                                             (* nop *)
  | Brop of brop * register * register * register   (* brop r r => r *)
  | Biop of biop * register * int * register        (* biop r n => r *)
  | Urop of urop * register * register              (* urop r => r *)
  | Load of register * register                     (* load r => r *)
  | LoadI of int * register                         (* loadI n => r *)
  | Store of register * register                    (* store r => r *)
  | Jump of label                                   (* jump l *)
  | CJump of register * label * label               (* cjump r l l *)

(* labelled blocks (lists of instructions) *)
type labelled_block = {
  label: label;
  statements: instruction list;
  edges: label list;
}

(* Grafo di controllo di flusso per MiniRISC *)
type risc_cfg = {
  blocks: (label * labelled_block) list;
  entry_node: label;
  terminal_node: label;
}

(* Contatori per generare etichette e registri unici *)
let label_counter = ref 0 (* L0, L1, L2... *)
let register_counter = ref 0 (* R0, R1, R2, ... *)

let assign_label () =
  let label = "L" ^ string_of_int !label_counter in (* converte il valore corrente del contatore in una stringa *)
  incr label_counter; (*  incrementa il contatore dopo ogni chiamata. *)
  label

let assign_register () =
  let register = "R" ^ string_of_int !register_counter in
  incr register_counter;
  register 

(* Funzione per tradurre un'espressione aritmetica MiniImp in una lista di istruzioni MiniRISC *)
let rec translate_a_exp (a_exp : MiniImp.a_exp) (reg_target : register) : instruction list =
  match a_exp with
  | Integer n -> [LoadI (n, reg_target)] 
  | Variable x -> [Load (x, reg_target)]
  | Add (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (Add, r1, r2, reg_target)]
  | Sub (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (Sub, r1, r2, reg_target)]
  | Mul (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (Mul, r1, r2, reg_target)]
  | Div (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (Div, r1, r2, reg_target)]
  | Mod (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (Mod, r1, r2, reg_target)]

(* Funzione per tradurre un'espressione booleana MiniImp in una lista di istruzioni MiniRISC *)
let rec translate_b_exp (b_exp : MiniImp.b_exp) (reg_target : register) : instruction list =
  match b_exp with
  | Boolean true -> [LoadI (1, reg_target)]
  | Boolean false -> [LoadI (0, reg_target)]
  | And (b1, b2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_b_exp b1 r1 @ translate_b_exp b2 r2 @ [Brop (And, r1, r2, reg_target)]
  | Or (b1, b2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_b_exp b1 r1 @ translate_b_exp b2 r2 @ [Brop (Or, r1, r2, reg_target)]
  | Not b1 ->
      let r1 = assign_register () in
      translate_b_exp b1 r1 @ [Urop (Not, r1, reg_target)]
  | LessThan (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (LessThan, r1, r2, reg_target)]
  | LessThanEqual (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (LessThanEqual, r1, r2, reg_target)]
  | GreaterThan (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (GreaterThan, r1, r2, reg_target)]
  | GreaterThanEqual (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (GreaterThanEqual, r1, r2, reg_target)]
  | Equal (a1, a2) ->
      let r1, r2 = assign_register (), assign_register () in
      translate_a_exp a1 r1 @ translate_a_exp a2 r2 @ [Brop (Equal, r1, r2, reg_target)]
 
(* Funzione per tradurre un nodo CFG MiniImp in un blocco MiniRISC *)
let translate_node (node : CFG.node) : labelled_block =
  let lbl = "L" ^ string_of_int node.id in
  let instrs =
    List.flatten (List.map (function
      | MiniImp.Skip -> [Nop]
      | MiniImp.Assign (x, a_exp) ->
          let reg_x = assign_register () in
          translate_a_exp a_exp reg_x @ [Store (reg_x, x)]
      | MiniImp.If (cond, _, _) ->
          let r_cond = assign_register () in
          translate_b_exp cond r_cond @
          [CJump (r_cond, "L" ^ string_of_int (List.nth node.edges 0), "L" ^ string_of_int (List.nth node.edges 1))]
      | MiniImp.While (cond, _) ->
          let r_cond = assign_register () in
          translate_b_exp cond r_cond @
          [CJump (r_cond, "L" ^ string_of_int (List.nth node.edges 0), "L" ^ string_of_int (List.nth node.edges 1))]
      | _ -> failwith "Non implementato"
    ) node.statements)
  in
  { label = lbl; statements = instrs; edges = List.map (fun id -> "L" ^ string_of_int id) node.edges }

(* Traduzione di un intero CFG di MiniImp a MiniRISC CFG *)
let trans_in_riscfg (cfg : CFG.cfg) : risc_cfg = 
  let blocks = List.map (fun (_, node) -> ("L" ^ string_of_int node.CFG.id, translate_node node)) cfg.nodes in
  { blocks; entry_node = "L" ^ string_of_int cfg.entry_node; terminal_node = "L" ^ string_of_int cfg.terminal_node }


(* Funzione per tradurre il CFG MiniRISC in codice Assembly MiniRISC *)
let instruction_to_string = function
  | Nop -> "nop"
  | Brop (op, r1, r2, r3) -> Printf.sprintf "%s %s %s => %s"
      (match op with
        | Add -> "add" | Sub -> "sub" | Mul -> "mult"
        | Div -> "div" | Mod -> "mod"
        | And -> "and" | Or -> "or"
        | LessThan -> "less" | LessThanEqual -> "less_eq"
        | GreaterThan -> "greater" | GreaterThanEqual -> "greater_eq" | Equal -> "equal"
      ) r1 r2 r3
  | Biop (op, r1, n, r2) -> Printf.sprintf "%s %s %d => %s"
      (match op with
        | AddInt -> "addI" | SubInt -> "subI" | MultInt -> "multI"
        | DivInt -> "divI" | ModInt -> "modI" | AndInt -> "andI" | OrInt -> "orI"
      ) r1 n r2
  | Urop (op, r1, r2) -> Printf.sprintf "%s %s => %s"
      (match op with Not -> "not" | Copy -> "copy") r1 r2
  | Load (r1, r2) -> Printf.sprintf "load %s => %s" r1 r2
  | LoadI (n, r) -> Printf.sprintf "loadI %d => %s" n r
  | Store (r1, r2) -> Printf.sprintf "store %s => %s" r1 r2
  | Jump lbl -> Printf.sprintf "jump %s" lbl
  | CJump (r, l1, l2) -> Printf.sprintf "cjump %s %s %s" r l1 l2

(* Controllo che nessun registro sia usato prima di essere definito *)
let check_uninitialized_registers (cfg : risc_cfg) : bool =
  let defined_registers = Hashtbl.create 50 in
  let errors = ref false in

  let check_instr (instr : instruction) =
    let use_regs, def_regs = match instr with
      | Brop (_, r1, r2, r3) -> ([r1; r2], [r3])
      | Biop (_, r1, _, r2) -> ([r1], [r2])
      | Urop (_, r1, r2) -> ([r1], [r2])
      | Load (_, r) -> ([], [r])
      | LoadI (_, r) -> ([], [r])
      | Store (r1, _) -> ([r1], [])
      | CJump (r, _, _) -> ([r], [])
      | _ -> ([], [])
    in

    (* Controlliamo se un registro viene usato prima di essere definito *)
    List.iter (fun r ->
      if not (Hashtbl.mem defined_registers r) then (
        Printf.printf "Errore: Il registro %s è usato prima di essere inizializzato!\n" r;
        errors := true
      )
    ) use_regs;

    (* Aggiungiamo i registri definiti *)
    List.iter (fun r -> Hashtbl.replace defined_registers r true) def_regs
  in

  (* Analizziamo ogni blocco nel CFG *)
  List.iter (fun (_, block) ->
    List.iter check_instr block.statements
  ) cfg.blocks;

  not !errors  (* Ritorna true se tutto è ok, false se ci sono errori *)











  
(* TARGET CODE GENERATION *)

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
    List.map (fun (lbl, block) -> 
      let new_instrs = List.flatten (List.map (translate_instruction memory_map kept_regs) block.instructions) in
      (lbl, { block with instructions = new_instrs })
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
