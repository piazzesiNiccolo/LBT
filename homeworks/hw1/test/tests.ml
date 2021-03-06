open Hw1.Ast

let testCount = 1000

(*
the predicate used for testing: evals an expressions and returns true if it raises a security violation

 *)
let isInt exp =  
  match Hw1.Interpreter.eval (Hw1.Env.empty_table ()) exp with 
  | Int _ | Error DivisionByZero -> true 
  | _ -> false


let isBool exp =  
  match Hw1.Interpreter.eval (Hw1.Env.empty_table ()) exp with 
  | Bool _  | Error DivisionByZero-> true 
  | _ -> false
let isInsecure exp =
  match exp |> Hw1.Interpreter.eval (Hw1.Env.empty_table ()) with
  | Error (SecurityViolation _) -> true
  | _ -> false

(*
We check two correlated properties: 

- insecure expressions should always results in a security violation error 

- secure expressions should never raise a false alarm

 *)
let testInts =
  QCheck2.Test.make ~name:"int expressions"
    ~count:testCount ~print:Print_utils.print_exp
    (Generators.genExp Generators.randomSmallSize []
       Generators.Tint)
    isInt 

let testBools =
  QCheck2.Test.make ~name:"bool expressions"
    ~count:testCount ~print:Print_utils.print_exp
    (Generators.genExp Generators.randomSmallSize []
       Generators.Tbool)
    isBool 
let testSecurityViolations =
  QCheck2.Test.make ~name:"security violations are always captured"
    ~count:testCount ~print:Print_utils.print_exp
    (Generators.genInsecureExp Generators.randomSmallSize []
       Generators.randomBaseType)
    isInsecure



let testSecureExpressions =
  QCheck2.Test.make ~name:"secure expressions never raises security violations"
    ~count:testCount ~print:Print_utils.print_exp
    (Generators.genSecureExp Generators.randomSmallSize []
       Generators.randomBaseType) (fun e -> not @@ isInsecure e)
;;

QCheck_runner.run_tests ~colors:true ~verbose:true
  [ testSecurityViolations; testSecureExpressions; testInts; testBools ]
