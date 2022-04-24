
(* 
EXAMPLES OF QCHECK

*)
open QCheck

open Hw1.Ast
(*type tree = Leaf of int | Node of tree * tree

let leaf x = Leaf x
let node x y = Node (x,y)




let tree_gen = QCheck.Gen.(sized @@ fix
                             (fun self n -> match n with
                                | 0 -> map leaf nat
                                | n ->
                                  
                                  frequency
                                    [1, map leaf nat;
                                     2, map2 node (self (n/2)) (self (n/2))]
                             ));;

(* generate a few trees, just to check what they look like: *)
QCheck.Gen.generate ~n:20 tree_gen;;
let test =
  QCheck.Test.make ~count:1000 ~name:"list_rev_is_involutive"
    QCheck.(list small_nat)
    (fun l -> List.rev (List.rev l) = l);;

let arbitrary_tree =
  let open QCheck.Iter in
  let rec print_tree = function
    | Leaf i -> "Leaf " ^ (string_of_int i)
    | Node (a,b) -> "Node (" ^ (print_tree a) ^ "," ^ (print_tree b) ^ ")"
  in
  let rec shrink_tree = function
    | Leaf i -> QCheck.Shrink.int i >|= leaf
    | Node (a,b) ->
      of_list [a;b]
      <+>
      (shrink_tree a >|= fun a' -> node a' b)
      <+>
      (shrink_tree b >|= fun b' -> node a b')
  in
  QCheck.make tree_gen ~print:print_tree ~shrink:shrink_tree;;
let rec mirror_tree (t:tree) : tree = match t with
  | Leaf _ -> t
  | Node (a,b) -> node (mirror_tree b) (mirror_tree a);;
(* we can check right now the property... *)
QCheck.Test.check_exn test;;

let test_buggy =
  QCheck.Test.make ~name:"buggy_mirror" ~count:200
    arbitrary_tree (fun t -> t = mirror_tree t);;
*)
let is_insecure exp = 
  match exp |> Hw1.Interpreter.eval (Hw1.Env.empty_table ()) with 
  | Error(SecurityViolation(_)) -> true 
  | _ -> false 

let test_security_violation = 
  QCheck2.Test.make ~name:"security violations are always captured" 
                    ~count:1000 
                    ~print:Print_utils.print_exp
                    Generators.generate_insecure_exp
                    is_insecure;;

let test_secure_expressions = 
  QCheck2.Test.make ~name:"secure expressions never raises security violations" 
                    ~count:1000 
                    ~print:Print_utils.print_exp
                    Generators.generate_secure_exp
                    (fun e -> not (is_insecure e));;

QCheck_runner.run_tests  ~colors:true ~verbose:true
[
  
  test_security_violation;
  test_secure_expressions;

];;