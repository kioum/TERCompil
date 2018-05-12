%{
  open Printf
  open Ast
  let mk_loc p e = {node = e; info = p}
%}
  
(* Déclaration des tokens *)
  
%token <int32> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT
%token <string> CONST_STRING 

%token PLUS MINUS MULT DIV MOD
%token AND OR
%token EQUAL NEQ LT LE MT ME
%token PT
%token NOT
%token PUN MUN

%token IF ELSE
%token FOR

%token SEMI

%token SET

%token INT BOOL STRING VOID
%token NULL

%token PRINTLN
%token NEW

%token COMMA
%token OP CP
%token OA CA
%token OC CC

%token INSTOF
%token CLASS
%token EXTENDS
%token PUBLIC
%token RETURN
%token STATIC
%token THIS

%token EOF 


(* Déclaration des priorités*)

%left OR
%left AND
%left EQUAL NEQ
%left LT LE MT ME INSTOF
%left PLUS MINUS
%left MULT DIV MOD
%right NOT
%left PT


(* SYMBOLE DE DÉPART DE LA GRAMMAIRE : ON DONNE SON NOM ET SON TYPE
   (QUI SERA LE TYPE DES PROGRAMMES DANS L'AST)
  IL FAUT REMPLACER 'unit' CI-DESSOUS PAR LE TYPE DE L'AST.
*)

%start prog
%type < 'info Ast.program > prog (* ? *)
    
%%

      (* RÈGLES DE GRAMMAIRE *)

prog:
   |cds=class_defs; mcd=main_class_def; EOF { (cds,mcd) }
;
  
main_class_def:
 | PUBLIC; CLASS; id=IDENT; OA;
PUBLIC; STATIC; VOID; main=IDENT; OP; STRING; arg=IDENT; OC; CC; CP;
b=bloc;CA {{name = id; params = arg ; instructions = b} }
;
class_defs:
 | cds=list(class_def) {cds}
;
class_def:
 | CLASS; id=IDENT; OA; dl=list(decl); CA; {{name_def=id; extends="";decls=dl;}}
 | CLASS; id=IDENT; EXTENDS; id2=IDENT; OA; dl=list(decl); CA; {{name_def = id; extends= id2; decls = dl}}
;
bloc:
 | OA; is=list(instr_) CA {is}
;

instr_:
 | i = instruction     { mk_loc ($startpos, $endpos) i }
;
instruction:
 (*| ie=instr_expr; SEMI {ie}  *)
 | ac=acces; SET; e=expr; SEMI {Iset(ac,e)}
 (* | id=IDENT; SEMI {id}*)
 | t=typ; id=IDENT; e=affectation {Idecl(t,id, e)}
 | IF; OP; e=expr; CP; b=bloc; {Iif(e,b)}
 | IF; OP; e=expr; CP; b1=bloc; ELSE; b2=bloc {Iifelse(e,b1,b2)}
 | FOR; OP; e1= option (expr); SEMI; e2=option(expr); SEMI; e3=option(expr); CP; b=bloc {Ifor(e1,e2,e3,b)}
 | b=bloc {Iblock(b)} 
(* | id=IDENT; MUN; SEMI {()}
 | id=IDENT; PUN; SEMI {()}
 | datt=decl_att; SEMI {()}
 | dmet=decl_meth; SEMI {()}
 | dconst=decl_constr; SEMI {()}
	  | NEW; id=IDENT; OP; l=separated_list(COMMA,expression); CP; SEMI {(*New(id,l)*)()}*)
 | RETURN; e= option(expr); SEMI {Ireturn(e)}
 | PRINTLN; OP; e= option(expr); CP;SEMI { Iprint(e)}
 | e = expr ; SEMI {Iexpr(e)}
;

affectation:
 | SET; e=expr; SEMI { Some e }
 | SEMI { None }
;

expr:
 |e = instr_expr {mk_loc ($startpos, $endpos) e}
;
instr_expr:
 (*| ap=appel; {EfunCall(ap)}*)
 | i = incr; ac=acces {Epreincr(i,ac) }
 | ac=acces; i=incr {Epostincr(ac,i)} (********************************)
 | lit=literal { Econst(lit) }
 (*| id=IDENT {id}*)
 (*| MINUS; lit=literal {Eunop(MINUS,lit)}*)
 | e1=expr; bop=binop; e2=expr { Ebinop(e1,bop, e2) }
 | up=unop; e=expr { Eunop(up, e) }
 | e=expr; INSTOF; id=IDENT {EinstOf(e, id)}
 | OP; t=typ; CP;e= expr {Ecast(t,e)}
 (*| ie=expr {ie}*)
 | ac=acces {Eaccess(ac)}
 (*| OP; e=expr; CP {e}************************************************)
 | NEW; id=IDENT; OP; l=separated_list(COMMA,expr); CP {Enew(id,l)}
 | i = instr_ {Einst(i)}
;
decl:
 | dc=decl_constr {dc}
 | dm=decl_meth {dm}
 | da=decl_att {da}
;
decl_constr:
 | id=IDENT; OP; dpars=separated_list(COMMA,param); CP; b=bloc {Constr(id,dpars,b)}
;
decl_meth:
 | t=typ; id=IDENT; OP; dats=separated_list(COMMA,param); CP; b=bloc {Meth(t,id,dats,b)}
;
decl_att:
 | t=typ; id=IDENT; SEMI; {Att(t,id)}
 ;
param:
 | t=typ; id=IDENT {(t,id)}
;
acces:
 | id=IDENT {Aident(id)}
 | e=expr; PT; id2=IDENT {Afield(e,id2)}
;
;
typ:
 | INT {TypInteger }
 | VOID {TypVoid } 
 | BOOL {TypBoolean}
 | STRING {TypClass "String"}
;
(*/////////////////////////////////////////////////*)

(*

instr_expr:
 (*| ac=acces; SET; e=expression{Eaccess(ac,e)}*)
 | ap=appel; {EfunCall(ap)}
 | i=incr; ac=acces {Epreincr(i,ac) }
 | ac=acces; i=incr {Epostincr(ac,i)}
 (*| ac=acces; i=incr {Epostincr(i,id)}
   | i=incr; ac=aces {Epreincr(i,id) }*)
(* | NEW; id=IDENT; OP; l=separated_list(COMMA,expression); CP {Enew(id,l)}*)
;


*)
(*/////////////////////////////////////////////////*)


(*/////////////////////////////////////////////////*)

(*
expression:
 | lit=literal { Literal(lit) }
 | id=IDENT {id}
 (*| NEQ; lit=literal {()}*)
 | MINUS; lit=literal {Eunop(MINUS,lit)}
 | e1=expression; bop=binop; e2=expression  { Binop(bop, e1, e2) }
 | e=expression; INSTOF; id=IDENT {e}
 | OP; t=typ; CP;e= expression {Ecast(t,e)}
 (*| OP; INT; CP; e=expression {Int e}
   | OP; BOOL; CP e=expression {Bool e}*)
 | ie=instr_expr {ie}
 | ac=acces {Eaccess(ac)}
 | OP; e=expression; CP {e}
;
*)
(*/////////////////////////////////////////////////*)


literal:
 | i=CONST_INT {Cint(i)}
 | b=CONST_BOOL {Cbool(b)}
 | s=CONST_STRING {Cstring(s)}
 | NULL {Cnull}
;

%inline binop:
    | PLUS   { Add }
    | MINUS  { Sub }
    | MULT   { Mult }
    | DIV    { Div }
    | MOD    { Modulo }
    | EQUAL  { Eq }
    | NEQ    { Neq }
    | LT     { Lt }
    | LE     { Le }
    | MT     { Mt }
    | ME     { Me }
    | AND    { And }
    | OR     { Or }
;

%inline incr:
    |PUN {Pun} (* ++ *)
    |MUN {Mun} (* -- *)

%inline unop:
    |NOT {Not}
    |MINUS {Neg}
