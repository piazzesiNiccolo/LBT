
type ide = string 

(* We model all binary operators with the type binop, so to handle them in a more concise manner inside the interpreter *)



type binop = Sum | Times | Minus | Divide | Equal | Less | Greater

type  event =  
    | Call of ide
    | Access of ide
    | Binop of binop
    
type error_kind = 
    |InvalidAccess of ide 
    |InvalidCall 
    |InvalidOperation

type exp = Eint of int
         | Ebool of bool
         | Den of ide 
         | Binop of binop*exp*exp
         | If of exp*exp*exp
         | Let of ide*exp*exp
         | Lambda of   ide list * exp  
         | SandboxExecute of  (event, exp) Security.sandbox
         | Call of exp*exp list

and fdecl = {
    fname: ide
    ; parameters: ide list
    ; body: exp
}
(*
The expression can be evaluated by the interpreter to an integer, a boolean, or a closure which is the runtime value of functions expressions.
 *)
and  value = Int of int
            | Bool of bool
            | Closure of  (ide list *exp*value Env.t)
            | Error of error
(* a closure is made of the function name, the function argument, the body of the function and the symbol table with the values captured. We assume for simplicity to have only one parameter per function,
   which is similar to function abstractions in the lambda calculus where we obtain multi parameter functions by chaining single parameter ones.*)
and error = 
    {
        kind: error_kind
        ; message: string
    }


type program = Prog of fdecl list * exp