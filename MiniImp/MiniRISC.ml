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
  | AddInt  (* addI *)
  | SubInt  (* subI *)
  | MultInt (* multI *)
  | DivInt  
  | ModInt  
  | AndInt  (* andI *)  
  | OrInt   
    
type urop = 
  | Not     (* not *)
  | Copy    (* copy *)                    

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
  instructions: instruction list;
  successors: label list;
}

(* Grafo di controllo di flusso per MiniRISC *)
type risc_cfg = {
  blocks: (label * labelled_block) list;
  entry: label;
  exit: label;
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
          [CJump (r_cond, "L" ^ string_of_int (List.nth node.successors 0), "L" ^ string_of_int (List.nth node.successors 1))]
      | MiniImp.While (cond, _) ->
          let r_cond = assign_register () in
          translate_b_exp cond r_cond @
          [CJump (r_cond, "L" ^ string_of_int (List.nth node.successors 0), "L" ^ string_of_int (List.nth node.successors 1))]
      | _ -> failwith "Non implementato"
    ) node.code)
  in
  { label = lbl; instructions = instrs; successors = List.map (fun id -> "L" ^ string_of_int id) node.successors }


(* Traduzione di un intero CFG di MiniImp a MiniRISC CFG *)let trans_in_riscfg (cfg : CFG.cfg) : risc_cfg = 
  let blocks = List.map (fun (_, node) -> ("L" ^ string_of_int node.CFG.id, translate_node node)) cfg.nodes in
  { blocks; entry = "L" ^ string_of_int cfg.entry; exit = "L" ^ string_of_int cfg.exit }


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
    List.iter check_instr block.instructions
  ) cfg.blocks;

  not !errors  (* Ritorna true se tutto è ok, false se ci sono errori *)
