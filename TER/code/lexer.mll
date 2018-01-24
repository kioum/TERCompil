{
  (* Analyse lexicale *)
  
  open Lexing
  open Parser
  
  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "true",     CONST_BOOL(true);
	"false",    CONST_BOOL(false);
	"if",       IF;
	"else",     ELSE;
	"int",      INT;
	"boolean",  BOOL;
	"class",    CLASS;
	"extends",  EXTENDS;
	"for",      FOR;
	"return",   RETURN;
	"new",      NEW;
	"public",   PUBLIC;
	"static",   STATIC;
	"this",     THIS;
	"void",     VOID;
	"instanceof", INSTOF;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
  
  (* Déclaration d'une exception *)
  exception Lexical_error of string
}

(* EXPRESSION RÉGULIÈRE POUR LES LETTRES *)

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
(* EXPRESSION RÉGULIÈRE POUR LES CHIFFRES *)
let ident = alpha (alpha | '_' )* digit?   
  
  (* RÈGLES RÉCURSIVES *)
  
  rule token = parse
(* PLACER TOUS LES AUTRES CAS ICI *)
  
  | '\n'
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "/*"
      { comment lexbuf; token lexbuf }
  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | "("
      { OP }
  | ")"
      { CP }
  | "["
      { OC }
  | "]"
      { CC }
  | "{"
      { OA }
  | "}"
      { CA }
  | ","
      { COMMA }
  | ";"
      { SEMI }
  | "="
      { SET }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "!"
      { NOT }
  | "*"
      { MULT }
  | "=="
      { EQUAL }
  | "!="
      { NEQ }
  | "++"
      { PUN }
  | "--"
      { MUN }
  | "<"
      { LT }
  | "<="
      { LE }
  | "&&"
      { AND }
  | "||"
      { OR }   
  | eof
      { EOF }
  | _
      {
	(* Quelque chose d'inconnu *)
	raise (Lexical_error "Caractère invalide")
      }
      
and comment = parse
    | ['\n']
	{ new_line lexbuf; comment lexbuf }
    | "/*"
	{ comment lexbuf; comment lexbuf }
    | "//"
	{ comment lexbuf; comment lexbuf }
    | "*/"
	{ () }
    | _
	{ comment lexbuf }
    | eof
{ failwith "Unterminated comment" }
