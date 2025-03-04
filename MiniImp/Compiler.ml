(*

Requisiti

Il compilatore dovrà:
✅ Accettare 3 input obbligatori:
1️⃣ Il numero di registri disponibili (n) nella macchina target.
2️⃣ Il file del programma MiniImp da compilare.
3️⃣ Il path in cui salvare il codice MiniRISC generato.

✅ Supportare 2 opzioni aggiuntive (attivabili/disattivabili dall'utente):
4️⃣ Controllo delle variabili non definite → Se attivato, il compilatore fallisce se una variabile non è inizializzata.
5️⃣ Ottimizzazione → Se attivata, applica la riduzione dei registri.




Lo suddividiamo in più passaggi:
1️⃣ Leggere il file MiniImp e convertirlo in un AST (MiniImp.program).
2️⃣ Costruire il CFG del programma MiniImp.
3️⃣ (Opzionale) Controllare variabili non inizializzate.
4️⃣ Convertire il CFG MiniImp in CFG MiniRISC.
5️⃣ (Opzionale) Ottimizzare il CFG MiniRISC.
6️⃣ Generare il codice Assembly MiniRISC e scriverlo su file.

*)


open MiniImp
open CFG
open MiniRISC
open MiniImpParser

(* Funzione principale del compilatore *)
let compile_miniimp_to_minirisc (n : int) (input_file : string) (output_file : string) 
                                (check_undef : bool) (optimize : bool) =
  (* 1️⃣ Leggiamo il file MiniImp e lo trasformiamo in un AST *)
  let program = parse_file input_file in

  (* 2️⃣ Costruiamo il CFG di MiniImp *)
  let cfg = build_cfg program in

  (* 3️⃣ Controllo delle variabili non inizializzate (se attivato) *)
  if check_undef then
    let input_var = match program with
      | Program (input, _, _) -> input
    in
    if not (check_uninitialized_variables cfg input_var) then
      failwith "Errore: il programma MiniImp usa variabili non definite!"
    else
      Printf.printf "✅ Controllo variabili superato!\n";

  (* 4️⃣ Convertiamo il CFG MiniImp in CFG MiniRISC *)
  let risc_cfg = trans_in_riscfg cfg in

  (* 5️⃣ Ottimizziamo il CFG MiniRISC (se attivato) *)
  let final_risc_cfg =
    if optimize then (
      Printf.printf "⚡ Ottimizzazione attivata: riduciamo il numero di registri...\n";
      translate_to_target risc_cfg n
    ) else risc_cfg
  in

  (* 6️⃣ Generiamo il codice MiniRISC e lo scriviamo su file *)
  let risc_code = trans_in_risc final_risc_cfg in
  let oc = open_out output_file in
  output_string oc risc_code;
  close_out oc;

  Printf.printf "✅ Compilazione completata! Il codice MiniRISC è stato salvato in: %s\n" output_file
