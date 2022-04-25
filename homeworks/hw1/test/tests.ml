
open QCheck

open Hw1.Ast

let is_insecure exp = 
  match exp |> Hw1.Interpreter.eval (Hw1.Env.empty_table ()) with 
  | Error(SecurityViolation(_)) -> true 
  | _ -> false 

let random_small_size = Gen.generate1 (Gen.oneofl [1;2;3;4;5;6;7;8;9;10])

let test_security_violation = 
  QCheck2.Test.make ~name:"security violations are always captured" 
                    ~count:1000
                    ~print:Print_utils.print_exp
                    (Generators.generateInsecureExp random_small_size [] Tint)
                    is_insecure;;

let test_secure_expressions = 
  QCheck2.Test.make ~name:"secure expressions never raises security violations" 
                    ~count:1000 
                    ~print:Print_utils.print_exp
                    (Generators.generateSecureExp random_small_size [] Generators.randomBaseType)
                    (fun e -> not (is_insecure e));;

QCheck_runner.run_tests  ~colors:true ~verbose:true
[
  
  test_security_violation;
  test_secure_expressions;

];;
