open Ast 
let rec  eval env  (expr: exp) =
  match expr with 
  | Eint n ->  Int n
  | Ebool b -> Bool b 
  | Den x -> x |> Env.lookup env
  | Binop (op, e1, e2) -> (
      let v1 = eval env e1 in 
      let v2 = eval env e2 in 
      match op,v1,v2 with 
      | Sum, Int n1, Int n2 -> Int (n1 + n2)
      | Minus, Int n1, Int n2 -> Int (n1 - n2)
      | Times, Int n1, Int n2 -> Int (n1 * n2)
      | Divide, Int n1, Int n2 -> Int (n1 / n2)
      | Equal, Int n1, Int n2 -> Bool (n1  = n2)
      | Less, Int n1, Int n2 -> Bool (n1  < n2)
      | Greater, Int n1, Int n2 -> Bool (n1  > n2)
      | _ -> Error {
          kind=InvalidOperation
        ; message = "invalid binary expressions"
        }

    )
  | If( e1, e2, e3) -> (
      match eval env e1 with 
      | Bool true -> eval env e2 
      | Bool false -> eval env e3 
      | _ -> Error {
          kind=InvalidOperation;
          message="if guard must be boolean"
        }

    )
  | Let (id, e1, e2 ) -> 
    let v = eval env e1 in 
    let new_env = Env.bind env id v in
    e2 |> eval new_env 
  | Lambda (i,e) -> Closure(i,e, env)
  | Call (f,args) -> 
    (match eval env f with
     | Closure(xs,body,env) -> 
        (*
        Calling a function involves the following steps: 
        - evaluating arguments passed 
        - updating the environment with the  new values for the arguments 
        - evalutating body of function with the new environment
         *)
       let x_vals = List.map (eval env) args in 
       let new_env = List.fold_left2 (Env.bind) env xs x_vals in
       eval new_env body
     |_ -> Error{
         kind=InvalidCall;
         message="Not calling a function"
       })  
  | _ -> failwith "Not implemented"


let add_func_decl env (f:fdecl) = 
    Closure(f.parameters,f.body,env)
   |>  Env.bind env f.fname 


let eval_program (Prog(funcs, exp)) =
    let init_env = Env.emptyenv in 
    let env = List.fold_left  (add_func_decl)  init_env funcs in
    eval env exp

