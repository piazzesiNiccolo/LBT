open Hw1

let rec err_to_string = function 
  | Ast.SecurityViolation e -> "SECURITY VIOLATION: " ^ err_to_string e
  | Ast.InvalidExecute -> "Cannot call execute from another execute"
  | Ast.InvalidAccess x -> "Cant access " ^ Option.get x
  | Ast.InvalidCall f -> "Invalid call of function " ^ Option.fold ~none:"anonymous" ~some:Fun.id f
  | Ast.InvalidExpression -> "Invalid expression"


let evaluate expr = 
  match  Interpreter.eval (Env.empty_table ()) expr with 
  | Ast.Int n -> Printf.printf "result: %d\n" n 
  | Ast.Bool b -> Printf.printf "%s" (string_of_bool b)
  | Ast.Error e -> err_to_string e |> Printf.printf "The following error occurred:\n %s \n" 
  | Ast.Closure(_,_,_,_) -> Printf.printf "Function"


let () = 
  let expr =Ast.Let("y",
                    Ast.Eint(3), 
                    Ast.SandboxExecute(
                      Call(
                        Ast.Den("fact"),
                        Ast.Den("y")),[Ast.Access("fact");Ast.Arith;Ast.Access("y")]))

  in 
  let fact = 

    Ast.Letfun("fact","n",Ast.If(
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
            Ast.Binop(Ast.Minus,Ast.Den("n"),Ast.Eint(1)))))
              ,expr)
  in
  evaluate fact
