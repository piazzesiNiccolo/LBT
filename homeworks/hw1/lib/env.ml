(*
The symbol table is here modelled as a simple hash table associating strings to a generic value. Here the value can be instantiated with different 
kind of data.
 *)

type 'v t = (string, 'v) Hashtbl.t

let  lookup (env:'v t) = Hashtbl.find_opt env

let empty_env = Hashtbl.create

let bind (env:'v t)  = Hashtbl.add env