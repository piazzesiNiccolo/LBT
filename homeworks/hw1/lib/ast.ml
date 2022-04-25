type ide = string

(* We model all binary operators with the type binop, so to handle them in a more concise manner inside the interpreter *)

type binop = Sum | Times | Minus | Divide | Equal | Less | Greater
type action = Access of ide | Arith | Execute

type exp =
  | Eint of int
  | Ebool of bool
  | Den of ide
  | Binop of binop * exp * exp
  | If of exp * exp * exp
  | Let of ide * exp * exp
  | Letfun of ide * ide * exp * exp
  | Lambda of ide * exp
  | SandboxExecute of exp * action list
  | Call of exp * exp

type error_kind =
  | SecurityViolation of error_kind
  | InvalidAccess of ide option
  | InvalidCall of ide option
  | InvalidExecute
  | DivisionByZero
  | InvalidExpression

(*
The expression can be evaluated by the interpreter to an integer, a boolean, or a closure which is the runtime value of functions expressions.
 *)
type value =
  | Int of int
  | Bool of bool
  | Closure of (ide option * ide * exp * value Env.t)
  | Error of error_kind
(* a closure is made of the function name, the function argument, the body of the function and the symbol table with the values captured. We assume for simplicity to have only one parameter per function,
   which is similar to function abstractions in the lambda calculus where we obtain multi parameter functions by chaining single parameter ones.*)
