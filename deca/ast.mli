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
  | Constr
  | Meth of typ * ident * (typ*ident) list * block
      
and typ =
  | TypInteger
  | TypBoolean
  | TypClass of ident
  | TypVoid
      
(* Un bloc de code est une liste d'instructions *)
and block = instruction list
  
and instruction = Skip
  | Set   of location   * expression                              (* Affectation       *)
  | For   of expression * expression * expression * block         (* Boucle For        *)
  | If    of expression * block * block                           (* Branchement       *)
  | ProcCall of call                                              (* Appel de fonction *)

and expression =
  | Literal   of literal                         (* Valeur immédiate    *)
  | Location  of location                        (* Valeur en mémoire   *)
  | Binop     of binop * expression * expression (* Opération binaire   *)
  | Unop      of binop * expression              (* Opération unaire    *)   
  | FunCall   of call                            (* Appel de fonction   *)
      
and call = string * expression list (* Appel de fonction *)
  
and literal =
  | Int  of int      (* Constante entière   *)
  | Bool of bool     (* Constante booléenne *)
  | String of string (* Constante String    *)
  | Null             (* variable null       *)

and location =
  | Identifier  of string   (* Variable en mémoire *)
  
and binop =
  | Add (* +  *) | Mult (* *  *) | Sub (* - *)
  | Eq  (* == *) | Neq  (* != *) | Modulo (* % *)
  | Lt  (* <  *) | Le   (* <= *) 
  | And (* && *) | Or   (* || *)

and unop =
  | Not (* ! *)   | Neg  (* -  *)

      (*
  | Pun  (* ++ *) | Mun  (* -- *) 
      *)
