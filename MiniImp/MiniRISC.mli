(* MiniRISC.mli *)
open CFG 

(* Identificatori per registri e etichette *)
type register = string
type label = string

(* Operazioni binarie, binarie immediate e unarie *)

(*(Binary Operations): Operazioni binarie su registri (Add, Sub, Mul, And, Less).*)
(* aggiunti: div, mod, lessthanequal, greaterthan, graterthanequal, equal*)
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
             
(* (Binary Immediate Operations): Operazioni binarie con costante (AddInt, SubInt, MultInt, AndInt). *)
type biop = 
  | AddInt  (* addI *)
  | SubInt  (* subI *)
  | MultInt (* multI *)
  | DivInt  (* divI, nuova operazione *)
  | ModInt  (* modI, nuova operazione *)
  | AndInt  (* andI *)  
  | OrInt   (* orI, nuova operazione *)
    
(* (Unary Operations): Operazioni unarie (Not, Copy). *)
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

(* labelled blocks (lists of instructions), Un blocco etichettato ha un'etichetta, una lista di istruzioni e una lista di successori. *)
type labelled_block = {
  label: label;
  instructions: instruction list;
  successors: label list;
}

(* Grafo di controllo di flusso per MiniRISC, l grafo di controllo di flusso MiniRISC (risc_cfg) contiene:
Una lista di blocchi (label * labelled_block).
Il nodo di ingresso (entry).
Il nodo di uscita (exit). *)
type risc_cfg = {
  blocks: (label * labelled_block) list;
  entry: label;
  exit: label;
}

(* Funzioni di utilità per la generazione di etichette e registri *)
val assign_label : unit -> label
val assign_register : unit -> register

val translate_a_exp : MiniImp.a_exp -> register -> instruction list
val translate_b_exp : MiniImp.b_exp -> register -> instruction list
val translate_node : CFG.node -> labelled_block
val trans_in_riscfg : CFG.cfg -> risc_cfg
val trans_in_risc : risc_cfg -> string

val check_uninitialized_registers : risc_cfg -> bool
