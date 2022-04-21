open Hw1

let rec err_to_string = function 
| Ast.SecurityViolation e -> "SECURITY VIOLATION: " ^ err_to_string e
| Ast.InvalidExecute -> "Cannot call execute from another execute"
|Ast.InvalidAccess x -> "Cant access " ^ Option.get x
|Ast.InvalidCall _-> "Invalid call of function"
|Ast.InvalidExpression -> "Invalid expression"


let evaluate expr = 
  match Interpreter.eval (Env.empty_table ()) expr with 
  | Ast.Int n -> Printf.printf "result: %d\n" n 
  | Ast.Bool b -> Printf.printf "%s" (string_of_bool b)
  | Ast.Error e -> err_to_string e |> Printf.printf "The following error occurred: %s \n" 
  | _ -> assert false
let () = 
  let expr =Ast.Let("x",
                      Ast.Eint(3), 
                      Ast.SandboxExecute(
                      Call(
                        Ast.Den("fact"),
                        [Ast.Den("x")]),[Ast.Access("fact");Ast.Arith;Ast.Access("x")]))
             
  in 
  let fact = 

    Ast.Letfun("fact",["n"],Ast.If(
        Ast.Binop(
          Ast.Equal,
          Ast.Den("n"),
          Ast.Eint(0)),
        Ast.Eint(1),
        Ast.Binop(
          Ast.Times,
          Ast.Den("n"),
          Ast.Call(
            Ast.Den("fact"),
            [Ast.Binop(Ast.Minus,Ast.Den("n"),Ast.Eint(1))])))
            ,expr)
  in
  evaluate fact
