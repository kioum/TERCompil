(* On programme est constitué de plusieurs fonctions, et on donne pour
   chacune son nom et sa définition. On pourrait aussi utiliser
   le type [function_info Symb_Tbl.t] *)
type program = class_info list * class_main

and class_main = {
  name: ident;
  params: ident;
  instructions: block;
}

and ident = string
  
(*info de chaque classe*)
and class_def = {
  name: ident;
  extends: ident;
  decls: decl;
}

and decl =
  | Att of typ * ident
  | Constr of ident * (typ * ident) list * block
  | Meth of typ * ident * (typ * ident) list * block
      
and typ =
  | TypInteger
  | TypBoolean
  | TypClass of ident
  | TypVoid
      
(* Un bloc de code est une liste d'instructions *)
and block = instruction list
  
and instruction =
    Skip
  | Set   of location * 'info expression                              (* Affectation       *)
  | For   of 'info expression * 'info expression * 'info expression * block         (* Boucle For        *)
  | If    of 'info expression * block * block                           (* Branchement       *)
  | If    of 'info expression * block
  | ProcCall of call                                              (* Appel de fonction *)

and 'info expr_ =
  | Econst    of const                         (* Valeur immédiate    *)
  | Eaccess    of access                        (* Valeur en mémoire   *)
  | Ebinop     of 'info expression * binop * 'info expression (* Opération binaire   *)
  | Eunop      of binop * 'info expression              (* Opération unaire    *)   
  | EfunCall   of call                            (* Appel de fonction   *)
  | EinstOf of 'info expression * ident
  | Enew of ident * 'info expression list
      
and call = string * expression list (* Appel de fonction *)
  
and const =
  | Cint  of int      (* Constante entière   *)
  | Cbool of bool     (* Constante booléenne *)
  | Cstring of string (* Constante String    *)
  | Cnull             (* variable null       *)

and access =
  | Aident
  | Athis
      
and binop =
  | Add (* +  *) | Mult (* *  *) | Sub (* - *)
  | Eq  (* == *) | Neq  (* != *) | Modulo (* % *)
  | Lt  (* <  *) | Le   (* <= *)
  | Mt  (* >  *) | Me   (* >= *) 
  | And (* && *) | Or   (* || *)

and unop =
  | Not (* ! *)   | Neg  (* -  *)

      (*
  | Pun  (* ++ *) | Mun  (* -- *) 
      *)
and ('a, 'b) node = {
  node: 'a;
  info: 'b;
}

and position = Lexing.position * Lexing.position

and 'info expression = ('info expr_, 'info) node

and parsed_prog = position prog
and typed_prog = typ prog
