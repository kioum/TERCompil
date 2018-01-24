{
  (* Analyse lexicale *)

  open Lexing
  open Parser

  (* Déclaration d'une exception *)
  exception Lexical_error of string

}

(* EXPRESSION RÉGULIÈRE POUR LES LETTRES *)

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
(* EXPRESSION RÉGULIÈRE POUR LES CHIFFRES *)


(* RÈGLES RÉCURSIVES *)

rule token = parse
     (* PLACER TOUS LES AUTRES CAS ICI *)


  | eof         { EOF }
  | _
                {
                        (* Quelque chose d'inconnu *)
                          raise (Lexical_error "Caractère invalide")
                 }
