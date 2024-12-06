open MiniImp

(* Assignment *)
let program =
  Program ("x", "y",(Assign ("y", (Integer 3))))
;;

try
  let result = eval_program program 1 in
  Printf.printf "Assignment result: y = %d\n" result
with
| Failure msg -> Printf.printf "Error: %s\n" msg
| Not_found -> Printf.printf "Error: Output variable not found\n"
| exn -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string exn)
;;

(* If *)
let program =
  Program ("x", "y",(If (Boolean true, Assign("y",(Integer 1)), Assign("y",(Integer 2)))))
;;

try
  let result = eval_program program 1 in
  Printf.printf "If result: y = %d\n" result
with
| Failure msg -> Printf.printf "Error: %s\n" msg
| Not_found -> Printf.printf "Error: Output variable not found\n"
| exn -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string exn)
;;


(* If *)
let program =
  Program ("x", "y",(While (GreaterThan(Variable "x", Integer 5), Assign("y", Sub(Variable "x", Integer 1)))))
;;

try
  let result = eval_program program 1 in
  Printf.printf "If result: y = %d\n" result
with
| Failure msg -> Printf.printf "Error: %s\n" msg
| Not_found -> Printf.printf "Error: Output variable not found\n"
| exn -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string exn)
;;

