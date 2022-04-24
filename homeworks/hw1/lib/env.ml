
exception DuplicateEntry

type 'a t = ((string, 'a) Hashtbl.t) list

(* Create an empty global base scope. *)
let empty_table () = [Hashtbl.create 0]


let new_scope table = Hashtbl.create 0 :: table


let close_scope table = List.tl table


let rec lookup  table symbol =
    match table with
        | []       -> None
        | (hd::tl) ->
            match Hashtbl.find_opt hd symbol with
                | None    -> lookup tl symbol 
                | Some(v) -> Some(v)

let bind table symbol info =
    let current = List.hd table in
    match Hashtbl.find_opt current symbol with
        | None    -> Hashtbl.add current symbol info; table
        | Some(_) -> raise DuplicateEntry