(* On programme est constitué de plusieurs fonctions, et on donne pour
   chacune son nom et sa définition. On pourrait aussi utiliser
   le type [function_info Symb_Tbl.t] *)
type 'info program = 'info class_def list * 'info class_main

and 'info class_main = {
  name: ident;
  params: ident;
  instructions: 'info block;
}

and ident = string
  
(*info de chaque classe*)
and 'info class_def = {
  name_def: ident;
  extends: ident;
  decls: 'info decl list;
}

and 'info decl =
  | Att of typ * ident
  | Constr of ident * (typ * ident) list * 'info block
  | Meth of typ * ident * (typ * ident) list * 'info block
      
and typ =
  | TypInteger
  | TypBoolean
  | TypClass of ident
  | TypVoid
  | TypNull
      
(* Un bloc de code est une liste d'instructions *)
and 'info block = 'info instruction list
  
and 'info instr =
    Iskip
  | Idecl of typ * ident * 'info expression option
  | Iblock of 'info block
  | Iset   of 'info access * 'info expression                              (* Affectation       *)
  | Ifor   of 'info expression option * 'info expression option * 'info expression option * 'info block        (* Boucle For        *)
  | Iifelse    of 'info expression * 'info block * 'info block                           (* Branchement       *)
  | Iif    of 'info expression * 'info block
  | IprocCall of 'info call                                              (* Appel de fonction *)
  | Iprint of 'info expression option
  | Ireturn of 'info expression option

and 'info expr_ =
  | Econst    of const                         (* Valeur immédiate    *)
  | Eaccess   of 'info access                        (* Valeur en mémoire   *)
  | Ebinop     of 'info expression * binop * 'info expression (* Opération binaire   *)
  | Eunop      of unop * 'info expression              (* Opération unaire    *)   
  | EfunCall   of 'info call                            (* Appel de fonction   *)
  | EinstOf of 'info expression * ident
  | Ecast of typ * 'info expression
  | Enew of ident * 'info expression list
  | Epreincr of incr * 'info access
  | Epostincr of 'info access * incr
      
and 'info call = string * 'info expression option list (* Appel de fonction *)
  
and const =
  | Cint  of int32    (* Constante entière   *)
  | Cbool of bool     (* Constante booléenne *)
  | Cstring of string (* Constante String    *)
  | Cnull             (* variable null       *)

and 'info access =
  | Aident of ident
  (*| Athis of ident
  | Acall of 'info call * ident
  | Aaccess of 'info access * ident
    | Aexpr of 'info expression * ident*)
  | Afield of 'info expression * ident
      
and binop =
  | Add (* +  *) | Mult (* *  *) | Sub (* - *)
  | Eq  (* == *) | Neq  (* != *) | Modulo (* % *)
  | Lt  (* <  *) | Le   (* <= *) | Div (* / *)
  | Mt  (* >  *) | Me   (* >= *) 
  | And (* && *) | Or   (* || *)

and unop =
  | Not (* ! *)   | Neg  (* -  *)

and incr =     
  | Pun  (* ++ *) | Mun  (* -- *) 
    
and ('a, 'b) node = {
  node: 'a;
  info: 'b;
}

and position = Lexing.position * Lexing.position

and 'info expression = ('info expr_, 'info) node
and 'info instruction = ('info instr, 'info) node
  
and parsed_prog = position program
and typed_prog = typ program
