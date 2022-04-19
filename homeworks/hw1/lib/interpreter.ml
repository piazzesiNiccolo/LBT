open Ast 
let eval env  (expr: exp) =
match expr with 
| Eint n ->  Int n
| Ebool b -> Bool b 
| Den x -> x |> Env.lookup env 
| _ -> failwith "Not implemented"
    