open Hw1.Ast
open Hw1
open QCheck
type ttype = Tint | Tbool | Tfun of ttype *ttype

type gamma =  (ttype,ide) Hashtbl.t 





(*let genNames = 
    QCheck.Gen.char_range 'a' 'z'
    |> QCheck.Gen.list

let trim_ctx = List.fold_left (fun acc e -> if List.exists (fun (x,_) -> x = fst e ) acc then acc else e::acc) [] 


let optOneofl = 
function
|[] ->[]
| xs -> [QCheck2.Gen.oneofl]


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
let int_lit n = Eint n

let bool_lit b = Ebool b
let exp = 
    SandboxExecute(
    Letfun(
        "f",
        "z",
    If(
        Call(Den("z"),Eint(3)),
        Binop(Sum,Eint(2),Eint(3)),
        Binop(Divide,Eint(4),Eint(1))
    ),
    Call(Den("f"),Den("a"))
    ),
    [Arith;Execute;Access("f");Access("z");Access("a")])

let generate_insecure_exp = QCheck2.Gen.pure exp

let generate_secure_exp =QCheck2.Gen.frequency[1,QCheck2.Gen.map int_lit QCheck2.Gen.nat; 1,QCheck2.Gen.map bool_lit QCheck2.Gen.bool]
