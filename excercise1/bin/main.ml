open Security_policy
open Ast

let no_write_after_read = 
  let m_delta state event = 
    match state,event with
    | 0,Security_policy.Read -> Some 1 
    | 1, Security_policy.Write -> Some 2 
    | 0, Security_policy.Write -> Some 0 
    | 1,Security_policy.Read -> Some 1 
    | 2, _ -> Some 2 
    | n, Security_policy.Open -> Some n
    | _ -> failwith "Invalid transition"
  in 
  {
    states=[0;1;2];
    init_state=0;
    delta=m_delta;
    accept_states=[0;1]

  }


let only_one_open = 
  let m_delta state event =  match state,event with 
    | 0,Security_policy.Open -> Some 1
    | 1, Security_policy.Open -> Some 2 
    | n, _ -> Some n 
  in 
  {
    states=[0;1;2];
    init_state=0;
    delta=m_delta;
    accept_states=[0;1]

  }

let no_write_allowed = 
  let m_delta state event =  match state,event with 
    | 0,Security_policy.Write -> Some 1
    | n, _ -> Some n 
  in 
  {
    states=[0;1;];
    init_state=0;
    delta=m_delta;
    accept_states=[0;]

  }


let get_result exp = 
  try 
    exp
    |> Interpreter.eval [] [] [only_one_open]
    |> Option.some
  with 
    Failure _ -> None
let () = 

  (* some simple tests to check functionality  *)

  let expressions = [
    ("simple function",Ast.Let("add_3",Ast.Fun("n",Ast.Binop(Ast.Sum,Ast.Den("n"),Ast.Eint(3))),Ast.Call(Ast.Den("add_3"),Ast.Eint(5))));
    ("write after read",Ast.LetEm(no_write_after_read,"x",Ast.Read("f"),Ast.Write("f")));
    (*bad case for no_write_after_read *)
    ("read after write",Ast.LetEm(no_write_after_read,"x",Ast.Write("f"),Ast.Read("f")));
    (*good case for no_write_after_read *)
    ("simple write",Ast.LetEm(no_write_allowed,"x",Ast.Eint(2),Ast.Write("f")));
    (*bad case for no_write_allowed *)
    ("simple read",Ast.LetEm(no_write_allowed,"x",Ast.Eint(2),Ast.Read("f")));
    (*good case for no_write_allowed*)
    ("two opens",Ast.Let("x",Ast.Open("f"),Ast.Open("f2")));
    (*bad case for no_two_open *)
    ("read after open",Ast.Let("x",Ast.Open("f"),Ast.Read("f")));
    (*good case for no_two_open *)

  ] 
  in 
  expressions
  |> List.map (fun (s,e) -> (s, get_result e))
  |> List.iter (fun (s,r)-> 
      let res_s = if Option.is_some r then 
          match Option.get r with 
            Int n -> string_of_int n 
          | Bool b -> string_of_bool b
          | Closure(_,_,_) -> "function"
        else "Security violation" in 
      Printf.printf "result of %s : %s\n\n" s res_s)

