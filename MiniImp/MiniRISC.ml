(* Project Fragment:
• Write a module for MiniRISC (syntax and simple statements,
the semantics is not required)
• Implement a translation from MiniImp CFG to MiniRISC CFG
• Implement a translation from MiniRISC CFG to MiniRISC
*)

open MiniImp
open CFG

(* Identifiers for registers and labels *)
type register = string
type label = string

(* Binary, unary, and immediate operations employed by MiniRISC instructions... *)
(* ... between virtual registers (e.g., add r1 r2 => r3). *)
type brop = 
  | Add 
  | Sub 
  | Mul 
  | Div 
  | Mod 
  | And 
  | Or
  | LessThan 
  | LessThanEqual 
  | GreaterThan 
  | GreaterThanEqual 
  | Equal
             
(* ... between a register and an immediate constant (e.g., addI r1 10 => r3). *)
type biop = 
  | AddInt 
  | SubInt 
  | MultInt 
  | DivInt 
  | ModInt 
  | AndInt 
  | OrInt   
    
(* ... between virtual registers (e.g., not r1 => r2). *)
type urop = 
  | Not 
  | Copy                    

(* MiniRISC instructions employing defined typed of operations *)
type instruction =
  | Nop
  | Brop of brop * register * register * register 
  | Biop of biop * register * int * register
  | Urop of urop * register * register
  | Load of register * register 
  | LoadI of int * register 
  | Store of register * register
  | Jump of label 
  | CJump of register * label * label 

(* Node in MiniRISC CFG, contains:
  - A label (unique)
  - A list of instructions
  - A list of successors *)
type labelled_block = {
  label: label;
  statements: instruction list;
  edges: label list;
}

(* The MiniRISC CFG, contains:
  - A list of labelled blocks (previously defined)
  - An entry node 
  - A terminal node *)
type risc_cfg = {
  blocks: (label * labelled_block) list;
  entry_node: label;
  terminal_node: label;
}

(* Counters to generate unique labels and registers *)
let label_counter = ref 0
let register_counter = ref 0

(* Utility functions to generate new labels and registers; 
they are constructed by appending a counter to a prefix string,
L for label + the unique integer and R for register + the unique integer *)
let new_label () =
  let label = "L" ^ string_of_int !label_counter in
  incr label_counter;
  label

let new_register () =
  let register = "R" ^ string_of_int !register_counter in
  incr register_counter;
  register 

(* Function to translate a MiniImp arithmetic expression (a_exp) into MiniRISC instructions *)
let rec aexp_in_risc (a_exp : a_exp) (register : register) : instruction list =
  match a_exp with
  (* Base case: load immediate or variable into the target register *)
  | Integer n -> [LoadI (n, register)] 
  | Variable x -> [Load (x, register)] 
  (* Recursive cases: for binary operations, evaluate both operands into new registers,
    then perform the operation and store the result in the target register 
    e.g. for Add(a1, a2): take a1, evaluate it into r1; take a2, evaluate it into r2; then do add r1 r2 => register *)
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
  (* Integer negation handled as 0 - a1; 
  creates a new register for 0, then subtracts a1 from it and stores in target register *)
  | NotInt a1 -> 
      let r1 = new_register() in
      let zero_reg = new_register() in
      aexp_in_risc a1 r1 @ [LoadI(0, zero_reg)] @ [Brop (Sub, zero_reg, r1, register)]

(* Function to translate a MiniImp boolean expression into MiniRISC instructions *)
let rec bexp_in_risc (b_exp : b_exp) (register : register) : instruction list =
  match b_exp with
  (* Base case: load boolean constants into the target register *)
  | Boolean true -> [LoadI (1, register)]
  | Boolean false -> [LoadI (0, register)]
  (* Recursive cases: for binary boolean operations, evaluate both operands into new registers,
    then perform the operation and store the result in the target register 
    e.g. for And(b1, b2): take b1, evaluate it into r1; take b2, evaluate it into r2; then do and r1 r2 => register *)
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
 
(* Function to translate a MiniImp CFG node into a MiniRISC block;
Seq is not present because is not put inside a single node because 
it is represented as a connection (an edge) between nodes *)
let node_to_block (node : node) : labelled_block =
  let label = "L" ^ string_of_int node.id in 
  let block_instructions =
    List.flatten (List.map (function
    (* Skip: no operation needed, translate to Nop *)
      | Skip -> [Nop]
    (* Assign: evaluate the arithmetic expression into a new register,
      then store that register's value into the variable's memory location *)
      | Assign (x, a_exp) ->
          let register = new_register () in
          aexp_in_risc a_exp register @ [Store (register, x)]
    (* If and While: evaluate the boolean condition into a new register,
      then perform a conditional jump (CJump) based on that register's value 
      to the appropriate successor labels *)
      | If (cond, _, _) ->
          let register = new_register () in
          bexp_in_risc cond register @
          (* CJump r Ltrue Lfalse *)
          [CJump (register, "L" ^ string_of_int (List.nth node.edges 0), "L" ^ string_of_int (List.nth node.edges 1))]
      | While (cond, _) ->
          let register = new_register () in
          bexp_in_risc cond register @
          [CJump (register, "L" ^ string_of_int (List.nth node.edges 0), "L" ^ string_of_int (List.nth node.edges 1))]
      | _ -> [] 
    ) node.statements)
  in
  (* Since assembly execution is linear, we must ensure control flows to the specific successor defined by the CFG edges. 
  If a block has exactly one successor and does not already end with a control transfer (like CJump), 
  we append an explicit Jump to prevent falling through to the wrong block. *)
  let check_jump =
    if block_instructions <> [] && List.length node.edges = 1 then
      match List.hd (List.rev block_instructions) with
      | CJump _ -> block_instructions
      | _ -> block_instructions @ [Jump ("L" ^ string_of_int (List.hd node.edges))]
    else
      block_instructions
  in
  { 
    label = label; 
    statements = check_jump; 
    edges = List.map (fun id -> "L" ^ string_of_int id) node.edges 
  }


(* Translation of a MiniImp CFG into a MiniRISC CFG;
  maps each MiniImp node to a MiniRISC labelled block, preserving the entry and terminal nodes 
  by converting their IDs to labels and constructing the blocks list. *)
let impcfg_in_riscfg (miniimp_cfg : cfg) : risc_cfg = 
  let minirisc_blocks = List.map (
    fun (_, miniimp_node) -> 
        let minirisc_block = node_to_block miniimp_node in 
        (minirisc_block.label, minirisc_block)
    ) miniimp_cfg.nodes in
      {   
        blocks = minirisc_blocks; 
        entry_node = "L" ^ string_of_int miniimp_cfg.entry_node; 
        terminal_node = "L" ^ string_of_int miniimp_cfg.terminal_node         
      }


(* Function to convert a MiniRISC instruction to its string representation for assembly output
e.g. Brop (Add, "R1", "R2", "R3") -> "add R1 R2 => R3" *)
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

(* Function to convert a MiniRISC CFG into its assembly string representation;
  iterates over each block and its instructions, formatting them appropriately 
  employing 'instruction_to_string' just defined *)
let riscfg_in_assembly (cfg : risc_cfg) : string =
  let buffer = Buffer.create 1024 in
  List.iter (fun (label, block) ->
    Buffer.add_string buffer (label ^ ":\n");
    List.iter (fun instr ->
      Buffer.add_string buffer ("  " ^ instruction_to_string instr ^ "\n")
    ) block.statements;
  ) cfg.blocks;
  Buffer.contents buffer


(* Target Code Generation
Project Fragment: 
- Implement a translation from MiniRISC CFG to MiniRISC for a target architecture: 
the number of registers must be an integerparameter (must work for n >= 4) 
*)

(* Helper function: 
it is called when translating an instruction and one of the source registers is spilled (not in kept_registers), 
so its value must be taken from memory into a temporaty physical register 'temp_register'.
The function returns such instructions to retrieve the value from memory. *)
let load_spilled (register : register) (register_map : int StringMap.t) (temp_register : register) : instruction list =
  let address = StringMap.find register register_map in
  [
    (* Load address from memory to temp_register, 
    then load value from that address into temp_register *)
    LoadI (address, temp_register);  
    Load (temp_register, temp_register)  
  ]

(* Helper function: 
it is called when translating an instruction and the destination register is spilled (not in kept_registers), 
so its value must be stored from a temporary physical register 'res_register' into memory.
The function returns such instructions to store the value into memory. *)
let store_spilled (register : register) (register_map : int StringMap.t) (res_register : register) (destination_register : register) : instruction list =
  let address = StringMap.find register register_map in
  [
    (* Load address into temp_addr_register,
    then store value from res_register into that address *)
    LoadI (address, destination_register); 
    Store (res_register, destination_register) 
  ]

(* Function to translate a single MiniRISC instruction considering 'n' registers;
it has in input:
- register_map: mapping of spilled registers to memory addresses
- kept_registers: list of registers to keep in physical registers
- temp_register1, temp_register2: temporary physical registers for loading/storing spilled registers
- instruction: the MiniRISC instruction to translate *)
let translate_instruction (register_map : int StringMap.t) (kept_registers : string list) 
                          (temp_register1 : register) (temp_register2 : register) (instruction : instruction) : instruction list =

  (* Helper function:
  for a given register, decides whether to use itself (if in kept_registers)
  or the temporary register (if spilled) *)
  let map_register register temp_register =
    if List.mem register kept_registers then register 
    else temp_register 
  in

  (* For each istruction, 
  use map_register to decide which physical register to use for each virtual register,
  and generate load/store instructions if needed for spilled registers 
  then reconstruct the instruction with the mapped registers. *)
  match instruction with
  (* add r1, r2 => r3 *)
  | Brop (op, r1, r2, r3) ->

    let r1_reg = map_register r1 temp_register1 in
    let r2_reg = map_register r2 temp_register2 in
    let r3_reg = map_register r3 temp_register1 in
      
    let load_r1 = if not (List.mem r1 kept_registers) 
      then load_spilled r1 register_map temp_register1 
      else [] 
    in let load_r2 = if not (List.mem r2 kept_registers) 
      then load_spilled r2 register_map temp_register2 
      else [] 
    in let store_r3 = if not (List.mem r3 kept_registers) 
        then store_spilled r3 register_map r3_reg temp_register2  
        else [] 
    in

    load_r1 @ load_r2 @ [Brop (op, r1_reg, r2_reg, r3_reg)] @ store_r3

  (* addI r1, 10 => r3 *)
  | Biop (op, r1, imm, r3) ->

    let r1_reg = map_register r1 temp_register1 in
    let r3_reg = map_register r3 temp_register1 in
      
    let load_r1 = if not (List.mem r1 kept_registers) 
      then load_spilled r1 register_map temp_register1 
      else [] 
    in let store_r3 = if not (List.mem r3 kept_registers) 
      then store_spilled r3 register_map r3_reg temp_register2 
      else [] 
    in
      
    load_r1 @ [Biop (op, r1_reg, imm, r3_reg)] @ store_r3

  (* not r1 => r2 *)
  | Urop (op, r1, r2) ->

    let r1_reg = map_register r1 temp_register1 in
    let r2_reg = map_register r2 temp_register1 in
      
    let load_r1 = if not (List.mem r1 kept_registers) 
      then load_spilled r1 register_map temp_register1 
      else [] 
    in let store_r2 = if not (List.mem r2 kept_registers) 
      then store_spilled r2 register_map r2_reg temp_register2 
      else [] 
    in
      
    load_r1 @ [Urop (op, r1_reg, r2_reg)] @ store_r2

  (* load r1 => r2 *)
  | Load (r_addr, r_dest) -> 

    let r_addr_reg = map_register r_addr temp_register1 in 
    let r_dest_reg = map_register r_dest temp_register2 in
      
    let load_addr = if not (List.mem r_addr kept_registers) 
      then load_spilled r_addr register_map temp_register1 
      else [] 
    in let store_dest = if not (List.mem r_dest kept_registers) 
      then store_spilled r_dest register_map r_dest_reg temp_register1 
      else [] 
    in
    
    load_addr @ [Load (r_addr_reg, r_dest_reg)] @ store_dest

  (* loadI 42 => r1 *)
  | LoadI (imm, r_dest) ->

    let r_dest_reg = map_register r_dest temp_register1 in
    let store_dest = if not (List.mem r_dest kept_registers) 
      then store_spilled r_dest register_map r_dest_reg temp_register2 
      else [] 
    in
      
    [LoadI (imm, r_dest_reg)] @ store_dest

  (* store r1 => r2 *)
  | Store (r_src, r_addr) ->
      
    let r_src_reg = map_register r_src temp_register1 in
    let r_addr_reg = map_register r_addr temp_register2 in
      
    let load_src = if not (List.mem r_src kept_registers) then 
      load_spilled r_src register_map temp_register1 
      else [] 
    in let load_addr = if not (List.mem r_addr kept_registers) then 
      load_spilled r_addr register_map temp_register2 
      else [] 
    in
      
    load_src @ load_addr @ [Store (r_src_reg, r_addr_reg)]

  (* cjump r5, l1, l2 *)
  | CJump (r, l1, l2) ->

    let r_reg = map_register r temp_register1 in
    let load_r = if not (List.mem r kept_registers) 
      then load_spilled r register_map temp_register1 
      else [] 
    in
      
    load_r @ [CJump (r_reg, l1, l2)]

  | Nop | Jump _ -> [instruction]  

(* Function to translate a MiniRISC CFG to a target MiniRISC CFG with n>=4 registers *)
let translate_to_target (cfg : risc_cfg) (n : int) : risc_cfg =
  if n < 4 then failwith "Needed 4 registers at least (2 for Brop, 2 for spilling)";
  let r_temp1 = "R_temp1" in
  let r_temp2 = "R_temp2" in

  (* Phase 1: Count register usage 
  'counting_table' is a hash table that for each register keeps track of how many times it has been used in the program;
  it is updated by 'update_count' that increments the count for a given register, ignoring temporary registers 
  and by 'update_count_list' that applies 'update_count' to a list of registers. *)
  let counting_table = Hashtbl.create 50 in
  let update_count register =
    if register <> r_temp1 && register <> r_temp2 then
      let count = (Hashtbl.find_opt counting_table register |> Option.value ~default:0) + 1 in
      Hashtbl.replace counting_table register count
  in
  let update_count_list l = List.iter update_count l in
  List.iter (fun (_, block) -> 
    List.iter (fun instr -> 
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

  (* Phase 2. Select n-2 most used registers 
  We sort the registers by descending counting and take top n-2 to keep in physical registers; 
  the other 2 are the temporary registers
  All these registers form 'kept_registers', while all other registers are spilled to memory ('spilled_registers') *)
  let sort_registers = 
    Hashtbl.fold (fun r count acc -> (r, count) :: acc) counting_table [] 
    |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1) 
    |> List.map fst
  in
  let sort_kept_registers = List.filteri (fun i _ -> i < (n - 2)) sort_registers in 
  let kept_registers = r_temp1 :: r_temp2 :: sort_kept_registers in 
  let spilled_registers = List.filter (fun r -> not (List.mem r kept_registers)) sort_registers in

  (* Phase 3. Create memory map for spilled registers assigning to each a unique memory address *)
  let memory_map =
    fst (List.fold_left (fun (acc, addr) r -> 
      (StringMap.add r addr acc, addr + 4))  
      (StringMap.empty, 1000) 
      spilled_registers)
  in

  (* Phase 4. Translate each block with `translate_instruction`;
  the blocks remain the same, only their instructions are translated *)
  let new_blocks = 
    List.map (fun (label, block) -> 
      let new_instrs = List.flatten (
        List.map (translate_instruction memory_map kept_registers r_temp1 r_temp2) 
        block.statements
      ) in
      (label, { block with statements = new_instrs })
    ) cfg.blocks
  in

  (* Phase 5. Return translated CFG *)
  { cfg with blocks = new_blocks }




  