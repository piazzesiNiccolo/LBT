
type ide = string 

(* We model all binary operators with the type binop, so to handle them in a more concise manner inside the interpreter *)



type binop = Sum | Times | Minus | Divide | Equal | Less | Greater


type exp = Eint of int
         | Ebool of bool
         | Den of ide 
         | Binop of binop*exp*exp
         | If of exp*exp*exp
         | Let of ide*exp*exp
         | Lambda of  ide option * ide * exp (* having an optional identifier as first element captures both named functions and lambdas *)
         | SandboxExecute of  (event, exp) Security.sandbox
         | ProofExecute of exp*Security.proof
         | Call of exp*exp

(*
The expression can be evaluated by the interpreter to an integer, a boolean, or a closure which is the runtime value of functions expressions.
 *)
and  value = Int of int
            | Bool of bool
            | Closure of  (ide*exp*value Env.t)
(* a closure is made of the function name, the function argument, the body of the function and the symbol table with the values captured. We assume for simplicity to have only one parameter per function,
   which is similar to function abstractions in the lambda calculus where we obtain multi parameter functions by chaining single parameter ones.*)

and  event =  
    | Call of ide
    | Access of ide
    | Binop of binop