open Hw1.Ast
open Hw1.Env
open Hw1.Interpreter

let rec err_to_string = function
  | SecurityViolation e -> "SECURITY VIOLATION: " ^ err_to_string e
  | DivisionByZero -> "cannot divide by zero"
  | InvalidExecute -> "Cannot call execute from another execute"
  | InvalidAccess x -> "Cant access " ^ Option.get x
  | InvalidCall f ->
      "Invalid call of function " ^ Option.fold ~none:"anonymous" ~some:Fun.id f
  | InvalidExpression -> "Invalid expression"

let evaluate (name, expr) =
  let res =
    match eval (empty_table ()) expr with
    | Int n -> string_of_int n
    | Bool b -> string_of_bool b
    | Error e -> "\nThe following error occurred: " ^ err_to_string e
    | Closure (_, _, _, _) -> "Function"
  in
  Printf.printf "EXPRESSION:%s \nRESULT:%s \n\n" name res

let () =
  let callFact =
    Let
      ( "y",
        Eint 6,
        SandboxExecute
          (Call (Den "fact", Den "y"), [ Access "fact"; Arith; Access "y" ]) )
  in

  let fact =
    Letfun
      ( "fact",
        "n",
        If
          ( Binop (Equal, Den "n", Eint 0),
            Eint 1,
            Binop
              (Times, Den "n", Call (Den "fact", Binop (Minus, Den "n", Eint 1)))
          ),
        callFact )
  in

  let mySumDef = Lambda ("x", Lambda ("y", Binop (Sum, Den "x", Den "y"))) in
  let mySumExec = Call (Call (Den "mySum", Eint 5), Eint 5) in

  let sendExec = Let ("result", Den "myPin", Call (Den "send", Den "result")) in
  let sendAllowed =
    Letfun
      ( "send",
        "x",
        Eint 0,
        Let
          ( "myPin",
            Eint 12345,
            SandboxExecute (sendExec, [ Access "myPin"; Access "send" ]) ) )
  in
  let sendNotAllowed =
    Letfun
      ( "send",
        "x",
        Eint 0,
        Let ("myPin", Eint 12345, SandboxExecute (sendExec, [ Access "myPin" ]))
      )
  in

  let expressions =
    [
      ( "simple function",
        Let
          ( "add_3",
            Lambda ("n", Binop (Sum, Den "n", Eint 3)),
            Call (Den "add_3", Eint 5) ) );
      ("factorial of 6", fact);
      ( "execute sum with binary_ops allowed",
        SandboxExecute
          (Call (Lambda ("x", Binop (Sum, Den "x", Eint 1)), Eint 5), [ Arith ])
      );
      ( "execute sum with binary_ops not allowed",
        SandboxExecute
          ( Call (Lambda ("x", Binop (Sum, Den "x", Eint 1)), Eint 5),
            [ Execute ] ) );
      ( "access mySum allowed",
        Let
          ( "mySum",
            mySumDef,
            SandboxExecute (mySumExec, [ Access "mySum"; Arith ]) ) );
      ( "access mySum not allowed",
        Let ("mySum", mySumDef, SandboxExecute (mySumExec, [ Arith ])) );
      ("send allowed", sendAllowed);
      ("send not allowed", sendNotAllowed);
    ]
  in

  List.iter evaluate expressions
