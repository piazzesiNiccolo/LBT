open Hw1.Ast 

let indent i = String.make (4 * i) ' ' 

let string_of_op = function 
| Sum -> "+"
| Minus -> "-"
| Times -> "*"
| Divide -> "/"
| Less -> "<"
| Greater -> ">"
| Equal -> "="

let string_of_action = function 
| Access x -> "access("^x^")"
| Execute -> "execute" 
| Arith -> "binary_ops"

let rec print_exp ?indent_level:(i=0) = function 
| Eint n -> indent i ^ string_of_int n 
| Ebool b -> indent i ^ string_of_bool b
| Den x -> indent i ^ x 
| Binop(op,e1,e2) -> indent i ^ "(" ^ print_exp e1 ^ string_of_op op ^ print_exp e2 ^ ")"
| Call(e1,e2) -> indent i ^ print_exp  e1 ^ "(" ^ print_exp e2 ^ ")"
| Lambda(id,e) -> indent i ^ "(fun "^ id ^ " -> " ^ print_exp e  ^ ")"
| If(e1,e2,e3) -> indent i ^ "if (" ^ print_exp e1 ^ ") then \n"
                ^ print_exp ~indent_level:(i+1) e2 ^"\n"
                ^ indent i ^ "else " ^ "\n"
                ^ print_exp ~indent_level:(i+1) e3 
| Let(id,e1,e2) -> indent i ^ "let " ^ id ^ " = \n"
                ^ print_exp  ~indent_level:(i+1) e1 ^ "\n"
                ^ indent i ^ "in \n"
                ^ print_exp  ~indent_level:(i+1) e2 
| Letfun(id,p,e1,e2) -> indent i ^ "let fun " ^ id ^ " " ^ p ^ " = \n"
                ^ print_exp  ~indent_level:(i+1) e1 ^ "\n"
                ^ indent i^ "in \n"
                ^ print_exp  ~indent_level:(i+1) e2 
        
| SandboxExecute (e,p) -> indent i ^  "execute \n" 
                ^ print_exp ~indent_level:(i+1) e 
                ^"\n" ^ indent i ^ "allowing [" ^String.concat ", " (List.map string_of_action p) ^ "]"
                
