open Ast 
open Security

let evaluate_binop op v1 v2 = 
  match op,v1,v2 with 
  | Sum, Int n1, Int n2 -> Int (n1 + n2)
  | Minus, Int n1, Int n2 -> Int (n1 - n2)
  | Times, Int n1, Int n2 -> Int (n1 * n2)
  | Divide, Int n1, Int n2 -> Int (n1 / n2)
  | Equal, Int n1, Int n2 -> Bool (n1  = n2)
  | Less, Int n1, Int n2 -> Bool (n1  < n2)
  | Greater, Int n1, Int n2 -> Bool (n1  > n2)
  | _, Error e , _ 
  | _, _,Error e -> Error e
  | _ -> Error (InvalidExpression)

let rec  eval (env:value Env.t)    = 

  function
  | Eint n ->  Int n
  | Ebool b -> Bool b 
  | Den x -> 
    x 
    |> Env.lookup env 
    |> Option.fold  
      ~none:(Error(InvalidAccess(Some x)))
      ~some:Fun.id
  | Binop (op, e1, e2) -> (
      let v1 = eval env  e1 in 
      let v2 = eval env  e2 in 
      evaluate_binop op v1 v2
    )
  | If( e1, e2, e3) -> (
      match eval env   e1 with 
      | Bool true -> eval env  e2 
      | Bool false -> eval env  e3 
      | Error e -> Error e
      | _ -> Error(InvalidExpression)

    )
  | Let (id, e1, e2 ) -> 
    ( match eval env  e1 with
      | Error e -> Error e 
      | v ->
        let new_env = Env.bind env id v in
        e2 |> eval new_env  )
  | Letfun(f,args,body,lbody) -> 
    (
      let benv = Closure(Some f,args,body,env)  |> Env.bind  env f in
      eval benv  lbody 
    )
  | Lambda (i,e) -> Closure(None,i,e, env)
  | Call (f,args) -> 
    (match eval env  f with 
     | Closure(f,xs,body,fdeclenv) -> 
        (*
        Calling a function involves the following steps: 
        - evaluating arguments passed 
        - updating the environment with the  new values for the arguments 
        - evalutating body of function with the new environment
         *)
       let x_vals = List.map (eval env  ) args in 
       let new_env = Env.new_scope fdeclenv in
       let fenv = List.fold_left2 (Env.bind) new_env xs x_vals  in
       eval fenv   body
     | Error e -> (
         let err = match e with
           |InvalidAccess f -> InvalidCall f
           | _ -> e in 
         Error err )
     |_ -> Error(InvalidAccess(None)))
  | SandboxExecute (e, permissions) -> 
    let sandbox = {
      permissions
    ; external_env=env
    ;internal_env = Env.empty_table ()
    } in 
    eval_in_sandbox  sandbox e




and eval_in_sandbox sandbox = 
  function 
  | Den x as e-> 
    let value = Env.lookup sandbox.internal_env x in 
    if Option.is_some value then 
      Option.get value 
    else 
    if allowed (Access(x)) sandbox  then 
      eval sandbox.external_env e
    else 
      Error(SecurityViolation(InvalidAccess(Some x)))
  | Binop(op,e1,e2) -> 
    if allowed Arith sandbox then 
      let v1 = eval_in_sandbox sandbox e1 in 
      let v2 = eval_in_sandbox sandbox e2 in 
      evaluate_binop op v1 v2
    else 
      Error(SecurityViolation(InvalidExpression))
  | If( e1, e2, e3) -> (
      match eval_in_sandbox sandbox  e1 with 
      | Bool true -> eval_in_sandbox sandbox  e2 
      | Bool false -> eval_in_sandbox sandbox  e3 
      | Error e -> Error e
      | _ -> Error(InvalidExpression)

    )
  | Let (id, e1, e2 ) -> 
    ( match eval_in_sandbox sandbox e1 with
      | Error e -> Error e 
      | v ->
        let new_env = Env.bind sandbox.internal_env id v in
        let upd_sandbox = {sandbox with internal_env=new_env} in
        e2 |> eval_in_sandbox upd_sandbox  )
  | Letfun(f,args,body,lbody) -> 
    (
      let benv = Closure(Some f,args,body,sandbox.internal_env)  |> Env.bind  sandbox.internal_env f in
      let upd_sandbox = {sandbox with internal_env=benv} in
      eval_in_sandbox upd_sandbox lbody
    )
  | Lambda (i,e) -> Closure(None,i,e, sandbox.internal_env)
  | SandboxExecute(e,_) -> 
    if allowed Execute sandbox then 
      let new_sandbox = {
        sandbox with
        permissions=sandbox.permissions;
        internal_env=Env.empty_table ()
      } in 
      eval_in_sandbox new_sandbox e
    else
      Error(SecurityViolation(InvalidExecute))
  | Call(f,args) -> 
    (match eval_in_sandbox sandbox  f with 
     | Closure(f,xs,body,fdeclenv) -> 
        (*
        Calling a function involves the following steps: 
        - evaluating arguments passed 
        - updating the environment with the  new values for the arguments 
        - evalutating body of function with the new environment
         *)
       let x_vals = List.map (eval_in_sandbox sandbox  ) args in 
       let new_env = Env.new_scope fdeclenv in
       let fenv = List.fold_left2 (Env.bind) new_env xs x_vals  in
       let upd_sandbox = {sandbox with internal_env = fenv} in
       eval_in_sandbox upd_sandbox  body

     | Error e -> (let err = match e with
         |InvalidAccess f -> InvalidCall f
         | SecurityViolation(InvalidAccess f) -> SecurityViolation(InvalidCall f)
         | _ -> e in 
        Error err )
     |_ -> Error(InvalidCall(None)))
  | e -> eval sandbox.internal_env e

