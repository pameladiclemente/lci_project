(* MiniRISC.mli *)
open MiniImp
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
  statements: instruction list;
  edges: label list;
}

(* Grafo di controllo di flusso per MiniRISC, l grafo di controllo di flusso MiniRISC (risc_cfg) contiene:
Una lista di blocchi (label * labelled_block).
Il nodo di ingresso (entry).
Il nodo di uscita (exit). *)
type risc_cfg = {
  blocks: (label * labelled_block) list;
  entry_node: label;
  terminal_node: label;
}

(* Funzioni di utilità per la generazione di etichette e registri *)
val new_label : unit -> label

val new_register : unit -> register

(* Funzione per tradurre un'espressione aritmetica MiniImp in una lista di istruzioni MiniRISC *)
val aexp_in_risc : MiniImp.a_exp -> register -> instruction list

(* Funzione per tradurre un'espressione booleana MiniImp in una lista di istruzioni MiniRISC *)
val bexp_in_risc : MiniImp.b_exp -> register -> instruction list

(* Funzione per tradurre un nodo CFG MiniImp in un blocco MiniRISC *)
val cfgnode_in_risc : CFG.node -> labelled_block

(* Traduzione di un intero CFG di MiniImp a MiniRISC CFG *)
val impcfg_in_riscfg : CFG.cfg -> risc_cfg

(* TODO, INSTRUCTION TO STRING? *)
val instruction_to_string : instruction -> string

(* Traduzione di MiniRISC CFG per un'architettura con `n` registri *)
val translate_to_target : risc_cfg -> int -> risc_cfg

(* Ottimizzazione: fusione dei registri basata sulla Liveness Analysis *)
val optimize_registers : risc_cfg -> string -> risc_cfg
