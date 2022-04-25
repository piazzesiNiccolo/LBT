open Hw1.Ast
module Gen = QCheck2.Gen

type ttype = Tint | Tbool | Tfun of ttype *ttype

type gamma =  (ide*ttype) list





(*let genNames = 
    QCheck.Gen.char_range 'a' 'z'
    |> QCheck.Gen.list

  let trim_ctx = List.fold_left (fun acc e -> if List.exists (fun (x,_) -> x = fst e ) acc then acc else e::acc) [] 




  let pickVar ctx typ = 
    let getVars ctx typ = 
    ctx 
    |> trim_ctx
    |> List.rev

    |> List.filter (fun (_,t) -> t=typ)
    |> List.map (fun (e,_)-> Ast.Den e)
    in 
    optOneofl(getVars ctx typ)

*)

let optOneofl = 
  function
  |[] ->[]
  | xs -> [QCheck2.Gen.oneofl xs]

let distinctVals l = List.fold_right (fun e acc -> if List.mem e acc then acc else e::acc) l []


let genIdent =  Gen.small_string ~gen:(Gen.char_range 'a' 'z')

let randomBaseType = Gen.generate1 (Gen.oneofl ([Tint; Tbool]))

let pickType (ctx:gamma) =
  ctx
  |> List.map snd 
  |> distinctVals 
  |> List.append [Tint;Tbool] 
  |> Gen.oneofl


let pickVar (ctx:gamma) (t:ttype) = 
  ctx 
  |> List.filter (fun (_,ty) -> t=ty )
  |> List.map fst 
  |> distinctVals 
  |> List.map (fun s -> Den s)
  |> optOneofl

let genPermission = Gen.oneof[
    Gen.map (fun s -> Access  s) (genIdent)
  ; Gen.pure Execute
  ; Gen.pure Arith
  ]
let generatePermissions = genPermission |> Gen.small_list

let intLit n = Eint n

let boolLit b = Ebool b

let genIntLit = Gen.map intLit Gen.nat
let genBoolLit = Gen.map boolLit Gen.bool 

let generate_secure_exp =Gen.frequency[1,genIntLit; 1,genBoolLit]

let rec genLambda size ctx t1 t2 = 
  let x = Gen.generate1 genIdent in 
  let gt = 
    Gen.generate1 
      (genTerm size ((x,t1)::ctx)  t2)
  in 
  Gen.pure (Lambda(x,gt))

and genBinOp size ctx t =
  let op = match t with 
    | Tint -> Gen.oneofl [Sum; Times; Minus;Divide]
    | Tbool -> Gen.oneofl [Less; Greater; Equal]
    | _ -> assert false
  in 
  let e1 = genTerm (size / 2) ctx Tint in 
  let e2 = genTerm (size / 2) ctx Tint in 
  Gen.pure(
    Binop(
      Gen.generate1 op,
      Gen.generate1 e1, 
      Gen.generate1 e2
    )
  )


and genIf size ctx t =  
  let guard = genTerm (size / 3) ctx Tbool in 
  let thenBranch = genTerm (size / 3) ctx t in 
  let elseBranch = genTerm (size / 3) ctx t in 
  Gen.pure (
    If(
      Gen.generate1 guard,
      Gen.generate1 thenBranch,
      Gen.generate1 elseBranch
    )
  )

and genLet size ctx t = 
  let x = Gen.generate1 (genIdent) in 
  let t1 = Gen.generate1 (pickType ctx) in 
  let xVal = genTerm (size / 2) ctx t1 in 
  let body = genTerm (size / 2) ((x,t1)::ctx) t in 
  Gen.pure(
    Let(
      x,
      Gen.generate1 xVal,
      Gen.generate1 body
    )
  ) 

and genLetFun size ctx t = 
  let f = Gen.generate1 (genIdent) in 
  let x = Gen.generate1 (genIdent) in 
  let t1 = Gen.generate1 (pickType ctx) in
  let t2 = Gen.generate1 (pickType ctx) in
  let funBody = genTerm (size / 2) ((f,Tfun(t1,t2))::(x,t1):: ctx) t2 in 
  let b = genTerm (size / 2) ((f,Tfun(t1,t2)):: ctx) t in 
  Gen.pure(
    Letfun(
      f,
      x,
      Gen.generate1 funBody,
      Gen.generate1 b
    )
  )
and genCall size ctx t = 
  let t1 = Gen.generate1 (pickType ctx) in 
  let funTerm = genTerm (size / 2) ctx (Tfun(t1,t)) in 
  let param = genTerm (size / 2) ctx t1 in 
  Gen.pure 
    (
      Call(
        Gen.generate1 funTerm,
        Gen.generate1 param
      )
    )


and genCompTerm size ctx t= Gen.oneof[
    genLet size ctx t;
    genLetFun size ctx t;
    genCall size ctx t;
    genIf size ctx t;

  ]

and genTerm size ctx  = 
  function
  | Tint -> 
    if size > 0 then 
      Gen.oneof ([
        genIntLit;
        genBinOp size ctx Tint;
        genCompTerm size ctx Tint;
      ] @ pickVar ctx Tint)
    else 
      Gen.oneof (genIntLit :: pickVar ctx Tint)
  | Tbool -> 
    if size > 0 then 
      Gen.oneof ([
        genBoolLit;
        genBinOp size ctx Tbool;
        genCompTerm size ctx Tbool;
      ] @ pickVar ctx Tbool)
    else 
      Gen.oneof (genBoolLit :: pickVar ctx Tbool)
  | Tfun(t1,t2) as t-> 
    if size > 0 then
      Gen.oneof ([
          genLambda size ctx t1 t2;
          genCompTerm size ctx t
        ] @ pickVar ctx t) 
    else 
      Gen.oneof (genLambda size ctx t1 t2 :: pickVar ctx t)

 
and generateSandboxExp ?insecure:(i=true) size ctx t= 
let p = Gen.generate1 (genPermission) in
let e  = match p with
| Access x -> Gen.pure (Den x)
| Execute -> 
  let e = Gen.generate1(genTerm (size/2) ctx t) in 
  Gen.pure (
   SandboxExecute(e,[])
  )
| Arith -> 
  let tb = randomBaseType in 
  genBinOp size ctx tb
in 
let perms = 
if i then
  Gen.generate1 generatePermissions |> List.filter (fun e -> e <> p)
else 
     p::Gen.generate1 generatePermissions   
in
 Gen.pure(
    SandboxExecute(
      Gen.generate1 e,
      perms
    )
  )

and generateInsecureExp size ctx t =  generateSandboxExp size ctx t


and generateSecureExp size ctx t =  
Gen.oneof[
genTerm size ctx t;
generateSandboxExp ~insecure:false size ctx t
]





let exp = 
  SandboxExecute(
    Letfun(
      "f",
      "z",
      If(
        Binop(Less,Eint(2),Eint(3)),
        Binop(Sum,Eint(2),Eint(3)),
        Binop(Divide,Eint(4),Eint(1))
      ),
      Gen.generate1 generate_secure_exp
    ),
    Gen.generate1 generatePermissions |> distinctVals)


