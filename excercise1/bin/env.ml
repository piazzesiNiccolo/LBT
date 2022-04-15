type 'v t = (string * 'v ) list 





let emptyenv = [];;

let rec lookup (env:'v t) x = 
  match env with 
  | [] -> failwith "Not found"
  | (ide, value)::r -> if x = ide then value else lookup r x;;

let bind (env:'v t) (x, v) = (x,v)::env;;