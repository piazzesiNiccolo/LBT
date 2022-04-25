open Hw1.Ast
open QCheck2.Gen


(*
Typing definitions. 
We define three possible types: Integers, Booleans and functions from t1 to t2. 

Gamma is the typing context, storing mappings from existing variables to their type. 

 *)
type ttype = Tint | Tbool | Tfun of ttype *ttype

type gamma =  (ide*ttype) list


let randomSmallSize = generate1 (oneofl [1;2;3;4;5]) (*small generator to return an integer between 1 and 5 *)

(* oneofL generator raises an exception when called with an empty list, we return an empty list instead *)
let optOneofl = 
  function
  |[] ->[]
  | xs -> [oneofl xs]

(* takes a list l and returns a new one without duplicates *)
let distinctVals l = List.fold_right (fun e acc -> if List.mem e acc then acc else e::acc) l []


(*generate random names to be used as variables  *)
let genIdent =  small_string ~gen:(char_range 'a' 'z')

let randomBaseType = generate1 (oneofl ([Tint; Tbool]))

let pickType (ctx:gamma) =
  ctx
  |> List.map snd 
  |> distinctVals 
  |> List.append [Tint;Tbool] 
  |> oneofl


let pickVar (ctx:gamma) (t:ttype) = 
  ctx 
  |> List.filter (fun (_,ty) -> t=ty )
  |> List.map fst 
  |> distinctVals 
  |> List.map (fun s -> Den s)
  |> optOneofl


(*generates random permissions for an execute expression *)
let genPermission = oneof[
    map (fun s -> Access  s) (genIdent)
  ; pure Execute
  ; pure Arith
  ]
let generatePermissions = genPermission |> small_list


(* Small generators for literal values *)
let intLit n = Eint n

let boolLit b = Ebool b

let genIntLit = map intLit nat
let genBoolLit = map boolLit bool 


let rec genLambda size ctx t1 t2 = 

  genIdent >>=
  (fun x -> map (fun e -> Lambda(x,e)) (genExp size ((x,t1)::ctx)  t2))

and genBinOp size ctx t =
  let op = match t with 
    | Tint -> oneofl [Sum; Times; Minus;Divide]
    | Tbool -> oneofl [Less; Greater; Equal]
    | _ -> assert false (*should never be called with Tfun *)
  in 
  let e1 = genExp (size / 2) ctx Tint in 
  let e2 = genExp (size / 2) ctx Tint in 
  map3 (fun op e1 e2 ->Binop(op,e1,e2)) op e1 e2


and genIf size ctx t =  
  let guard = genExp (size / 3) ctx Tbool in 
  let thenBranch = genExp (size / 3) ctx t in 
  let elseBranch = genExp (size / 3) ctx t in 
  map3  (fun g e1 e2 ->If(g,e1,e2)) guard thenBranch elseBranch

and genLet size ctx t = 
  let newVar = (and+) genIdent  (pickType ctx) in
  newVar >>= (fun (x,t1) ->
      let xVal = genExp (size / 2) ctx t1 in 
      let body = genExp (size / 2) ((x,t1)::ctx) t in
      map2 (fun xVal body -> Let(x,xVal,body)) xVal body
    )
and genLetFun size ctx t = 
  genIdent >>= 
          (fun f -> 
            genIdent >>=
              (fun x ->
                let pickTypes = (and*) (pickType ctx) (pickType ctx) in 
                pickTypes >>= 
                  (fun (t1,t2) -> 
                  let funBody = genExp (size / 2) ((x,t1):: ctx) t2 in 
                  let b = genExp (size / 2) ((f,Tfun(t1,t2)):: ctx) t in 
                  map2 (fun funBody b -> Letfun(f,x,funBody,b)) funBody b
)))

  
and genCall size ctx t = 
  pickType ctx >>= (fun t1 ->
      let funExp = genExp (size / 2) ctx (Tfun(t1,t)) in 
      let param = genExp (size / 2) ctx t1 in 
      map2 
        ( fun funExp param ->
            Call(
              funExp,
              param
            )
        ) funExp param
    )

and genCompExp size ctx t= oneof [
    genLet size ctx t;
    genLetFun size ctx t;
    genCall size ctx t;
    genIf size ctx t;

  ]

and genExp size ctx  = 
  function
  | Tint -> 
    if size > 0 then 
      frequency ([
          2,genIntLit;
          2,genBinOp size ctx Tint;
          1,genCompExp size ctx Tint;
        ] @ List.map (fun v -> 1,v) (pickVar ctx Tint))
    else 
      oneof (genIntLit :: pickVar ctx Tint)
  | Tbool -> 
    if size > 0 then 
      frequency ([
          2,genBoolLit;
          2,genBinOp size ctx Tbool;
          1,genCompExp size ctx Tbool;
        ] @ List.map (fun v -> 1,v) (pickVar ctx Tbool))
    else 
      oneof (genBoolLit :: pickVar ctx Tbool)
  | Tfun(t1,t2) as t-> 
    if size > 0 then
      frequency ([
          2,genLambda size ctx t1 t2;
          1,genCompExp size ctx t
        ] @ List.map  (fun v -> 1,v) (pickVar ctx t))
    else 
      oneof (genLambda size ctx t1 t2 :: pickVar ctx t)

and  filterPerms i p permissions = 
  (* 
if we're generating insecure expression we remove the generated permission, otherwise we make sure that it is present 
*)
  if i then
    permissions |> List.filter (fun e -> e <> p) |> distinctVals
  else 
    Arith::Execute::p::permissions  |> distinctVals
and  getAccessExp x = Den x 


(*
To test the security properties we create a bunch of simple, but security relevant programs: 

- To test invalid access, we simply create some expressions that only access an invalid variable. It does not matter 
if the variable has a value in the external environment, because the interpreter must raise a security violation 
before trying to access it. 

- To test invalid nested executions, we simply generate a random normal expression and wrap it in an execute call. Since 
the nested permission must be ignored we can put an empty list

- To test invalid use of arithmetics we generate a random binary operation that evaluates to an integer or a boolean
 *)
and generateSandboxExp ?insecure:(i=true) size ctx t= 
  let tb = randomBaseType in
  let exprGens = (and+) (genExp (size/2) ctx t) (genBinOp size ctx tb) in 
  map3 (fun (e1,e2) p perms ->
      let e  = match p with
        | Access x -> Den x
        | Execute -> 
          SandboxExecute(e1,[])

        | Arith -> 
          e2

      in
      SandboxExecute(
        e,
        filterPerms i p perms
      )
    ) exprGens genPermission generatePermissions


(* by default generateSandboxExp creates insecure expressions so we can simply call it *)
and generateInsecureExp size ctx t =  generateSandboxExp size ctx t

(*
A secure expression is a normal expression not wrapped in an execute, 
or an execute expression with the right permissions set up
 *)

and generateSecureExp size ctx t =  
  oneof[
    genExp size ctx t;
    generateSandboxExp ~insecure:false size ctx t
  ]








