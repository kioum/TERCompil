(* Syntaxe abstraite typée *)
module Symb_Tbl = Map.Make(String)
  
(* On programme est constitué de plusieurs fonctions, et on donne pour
   chacune son nom et sa définition. On pourrait aussi utiliser
   le type [function_info Symb_Tbl.t] *)
type program = (string * class_info) list

(*info de chaque classe*)
and class_info = {
  extends: class_info;
  attributs: (string * typ) list;
  functions: function_info Symb_Tbl.t;
}
  
and function_info = {
  (* Les mentions du type de retour et des types des paramètres seront
     utiles pour la vérification des types *)
  return:  typ option;
  formals: (string * typ) list;
  locals:  identifier_info Symb_Tbl.t;
  code:    block;
}
  
(* Différentes sortes de variables *)
and identifier_kind =
  | Local         (* Variable locale  *)
  | Formal of int (* Paramètre formel *)
  | Return        (* Résultat d'une fonction *)
and identifier_info = { typ: typ; kind: identifier_kind }
and typ =
  | TypInteger
  | TypBoolean
  | TypObject of class_info
      
(* Un bloc de code est une liste d'instructions *)
and block = instruction list
  
and instruction =
  | Set   of location   * expression                              (* Affectation       *)
  | For   of expression * expression * expression * block         (* Boucle For        *)
  | If    of expression * block * block                           (* Branchement       *)
  | ProcCall of call                                              (* Appel de fonction *)

and expression =
  | Literal   of literal                         (* Valeur immédiate    *)
  | Location  of location                        (* Valeur en mémoire   *)
  | binop     of binop * expression * expression (* Opération binaire   *)
  | Unop      of binop * expression              (* Opération unaire    *)   
  | FunCall   of call                            (* Appel de fonction   *)
      
and call = string * expression list (* Appel de fonction *)
  
and literal =
  | Int  of int  (* Constante entière   *)
  | Bool of bool (* Constante booléenne *)

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
