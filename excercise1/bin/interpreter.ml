(*exp as last argument in order to exploit partial function application to use the |> operator*)

open Ast

let get_trace expr = 
  (* helper function to associate relevant expressions to the list of events that they produce *)
  match expr with 
  | Read _ -> [Security_policy.Read]
  | Write _ -> [Security_policy.Write]
  | Open _ -> [Security_policy.Open]
  | _ -> []


(*
The following changes are made to eval to accomodate for the new security policy:

  - we take as parameter the trace of events and the list of security policies to check 
  - the trace of events is used to keep track of relevant actions to check in security policies 
  - Read, Write and open operations are checked to ensure they do not violate security policies. The check consists of evaluating the automata with the current execution trace. 
  This is a very simple but inefficient way to check them, since the automata gets entirely rerun at every operation. An optimization would be to inline the automatas as code directly,
  adding variables and code for their native execution and failure handling
 *)

let rec eval env   (trace: Security_policy.event list) (policies: Security_policy.policy list) (expr:exp)  = 
  match expr with 
  |Eint(n) -> Int n
  |Ebool b -> Bool b
  |Fun(x, body) -> Closure(x,body,env)
  |Den x -> x |> Env.lookup env 
  |If(e1,e2,e3) -> 
    (match e1 |> eval env   trace policies with 
     | Bool true -> eval env trace policies e3
     | Bool false -> eval env trace policies e2
     | _ -> failwith "If guard must evaluate to a boolean") 
  |Let(id, e1, e2) -> let v1 = e1 |> eval  env trace policies  in
    let add_t = get_trace e1 in 
    e2 |> eval ((id,v1)|> Env.bind env) (trace @ add_t) policies
  |LetEm (p,id, e1, e2) -> let new_p = p::policies in 
    let v = e1 |> eval env trace  new_p in 
    let add_t = get_trace e1 in
    e2 |> eval ((id,v) |> Env.bind env) (trace @ add_t) new_p
  (* in the execution monitor version of let we use the stricter semantics where the policy gets evaluated for both subexpressions in a let *)
  |Binop(op,e1,e2) -> (
      let e1 = eval env trace policies e1  in 
      let e2 = eval env trace policies e2 in
      match op,e1,e2 with 
      | Sum,Int n1, Int n2 -> Int (n1+n2)
      | Times,Int n1, Int n2 -> Int (n1 * n2)
      | Minus,Int n1, Int n2 -> Int (n1-n2)
      | Equal, Int n1, Int n2 -> Bool (n1 = n2)
      | Less, Int n1, Int n2 -> Bool (n1 < n2)
      | Greater, Int n1, Int n2 -> Bool (n1 > n2)
      | _ -> failwith "Invalid binary expression")  
  |Call(f, arg) -> (match eval env trace policies f with 
      | Closure(x,body,env) -> 
        let x_val = arg |> eval  env trace policies  in 
        let add_t = get_trace arg in 
        body |> eval ((x, x_val) |> Env.bind env) (trace @ add_t) policies  
      | _ -> failwith "Not calling a function")
  (* Read Write Open does not do any concrete operation to simplify reasoning *)
  | Read  _ -> if (Security_policy.check_policies (trace @ [Security_policy.Read]) policies) then Bool true 
    else failwith "Invalid read"
  | Write  _ -> if (Security_policy.check_policies (trace @ [Security_policy.Write]) policies) then Bool true 
    else failwith "Invalid write"
  | Open _ -> if (Security_policy.check_policies (trace @ [Security_policy.Open]) policies) then Bool true 
    else failwith "Invalid open"








