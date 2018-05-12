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
	"String",   STRING;
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
	"null",     NULL
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
let ident = alpha (alpha | '_' | '\'' | digit)*
  
  (* RÈGLES RÉCURSIVES *)
  
  rule token = parse
(* PLACER TOUS LES AUTRES CAS ICI *)
  
  | '\n'
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "/*"
      { comment lexbuf; token lexbuf }
  | "//"
      { comment_line lexbuf; token lexbuf }     
  | digit+
      { CONST_INT (Int32.of_string (lexeme lexbuf)) }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | '"'
      { lire_string (Buffer.create 17) lexbuf }
  | "System.out.println"
      { PRINTLN }
  | "."
      { PT }    
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
  | "%"
      { MOD }
  | "="
      { SET }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "/"
      { DIV }
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
  | ">"
      { MT }
  | ">="
      { ME }
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
      
and lire_string buf = parse
    | '"'
	{ CONST_STRING (Buffer.contents buf) }
    | '\\' '/'
	{ Buffer.add_char buf '/'; lire_string buf lexbuf }
    | '\\' '\\'
	{ Buffer.add_char buf '\\'; lire_string buf lexbuf }
    | '\\' 'b'
	{ Buffer.add_char buf '\b'; lire_string buf lexbuf }
    | '\\' 'f'
	{ Buffer.add_char buf '\012'; lire_string buf lexbuf }
    | '\\' 'n'
	{ Buffer.add_char buf '\n'; lire_string buf lexbuf }
    | '\\' 'r'
	{ Buffer.add_char buf '\r'; lire_string buf lexbuf }
    | '\\' 't'
	{ Buffer.add_char buf '\t'; lire_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      lire_string buf lexbuf
    }
  | _
      { raise (Lexical_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof
      { raise (Lexical_error ("String is not terminated")) }
  
and comment = parse
    | ['\n']
	{ new_line lexbuf; comment lexbuf }
    | "/*"
	{ comment lexbuf; comment lexbuf }
    | "*/"
	{ () }
    | _
	{ comment lexbuf }
    | eof
	{ failwith "Unterminated comment" }

and comment_line = parse
    | ['\n']
	{ () }
    | "/*"
	{ comment_line lexbuf; comment_line lexbuf }
    | "*/"
	{ () }
    | _
	{ comment_line lexbuf }
    | eof
	{ failwith "Unterminated comment" }
