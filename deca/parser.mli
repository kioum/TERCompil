
(* The type of tokens. *)

type token = 
  | VOID
  | THIS
  | STRING
  | STATIC
  | SET
  | SEMI
  | RETURN
  | PUN
  | PUBLIC
  | PT
  | PRINTLN
  | PLUS
  | OR
  | OP
  | OC
  | OA
  | NULL
  | NOT
  | NEW
  | NEQ
  | MUN
  | MULT
  | MT
  | MOD
  | MINUS
  | ME
  | LT
  | LE
  | INT
  | INSTOF
  | IF
  | IDENT of (string)
  | FOR
  | EXTENDS
  | EQUAL
  | EOF
  | ELSE
  | DIV
  | CP
  | CONST_STRING of (string)
  | CONST_INT of (int32)
  | CONST_BOOL of (bool)
  | COMMA
  | CLASS
  | CC
  | CA
  | BOOL
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ( 'info Ast.program )
