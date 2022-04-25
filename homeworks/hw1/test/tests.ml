

open Hw1.Ast

let is_insecure exp = 
  match exp |> Hw1.Interpreter.eval (Hw1.Env.empty_table ()) with 
  | Error(SecurityViolation(_)) -> true 
  | _ -> false 


let test_security_violation = 
  QCheck2.Test.make ~name:"security violations are always captured" 
                    ~count:10000
                    ~print:Print_utils.print_exp
                    (Generators.genInsecureExp Generators.randomSmallSize [] (Generators.randomBaseType))
                    is_insecure;;

let test_secure_expressions = 
  QCheck2.Test.make ~name:"secure expressions never raises security violations" 
                    ~count:10000
                    ~print:Print_utils.print_exp
                    (Generators.genSecureExp Generators.randomSmallSize [] (Generators.randomBaseType))
                    (fun e -> not (is_insecure e));;

QCheck_runner.run_tests  ~colors:true ~verbose:true
[
  
  test_security_violation;
  test_secure_expressions;

];;
