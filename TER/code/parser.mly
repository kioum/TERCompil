%{
(* let mk_loc p e = *)
  open Printf
  open Ast
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

%token VAR
%token INT BOOL STRING VOID
%token NULL

%token PRINT
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


%right SET
%left OR
%left AND
%left EQUAL NEQ
%left LT LE MT ME INSTOF
%left PLUS MINUS
%left MULT DIV MOD
%right NOT PUN MUN NEG
%left PT


(* SYMBOLE DE DÉPART DE LA GRAMMAIRE : ON DONNE SON NOM ET SON TYPE
   (QUI SERA LE TYPE DES PROGRAMMES DANS L'AST)

  IL FAUT REMPLACER 'unit' CI-DESSOUS PAR LE TYPE DE L'AST.
*)

%start prog
%type < 'info program > prog (* ? *)
    
%%

      (* RÈGLES DE GRAMMAIRE *)

prog:
   |cds=class_defs; mcd=main_class_def; EOF { (cds,mcd) }
;
  
main_class_def:
 | PUBLIC; CLASS; id=IDENT; OA;
PUBLIC; STATIC; VOID; main=IDENT; OP; str=IDENT; arg=IDENT; OC; CC; CP;
b=bloc;CA {{name = id; params = arg ; instructions = b} }
;
class_defs:
 | cds=list(class_def) {cds}
;
class_def:
 | CLASS; id=IDENT; OA; dl=list(decl); CA; {{name=id; extends=null;decls=dl;}}
 | CLASS; id=IDENT; EXTENDS; id2=IDENT; OA; dl=list(decl); CA; {{name = id; extends= id2}}
;
bloc:
 | OA; is=instructions; CA {is}
;
instructions:
 | inst=list(instruction) {inst}
;
instruction:
 | ie=instr_expr; SEMI {ie}	   
 | id=IDENT; SET; e=expression; SEMI {Iset(id,e)}
 | id=IDENT; SEMI {id}
 | t=typ; id=IDENT; SEMI {Att(t,id)}
 | t=typ; id=IDENT; SET; e=expression; SEMI {(t,id,e)}
 | IF; OP; e=expression; CP; i=instruction; {If(e,i)}
 | IF; OP; e=expression; CP; i1=instruction; ELSE; i2=instruction {Ifelse(e,i1,i2)}
 | FOR; OP; e1=expression; COMMA; e2=expression; COMMA; e3=expression; CP; i=instruction {For(e1,e2,e3,i)}
 | b=bloc {b} 
(* | id=IDENT; MUN; SEMI {()}
 | id=IDENT; PUN; SEMI {()}
 | datt=decl_att; SEMI {()}
 | dmet=decl_meth; SEMI {()}
 | dconst=decl_constr; SEMI {()}
	  | NEW; id=IDENT; OP; l=separated_list(COMMA,expression); CP; SEMI {(*New(id,l)*)()}*)
 | RETURN; e=expression; SEMI {Ireturn(e)}
 | RETURN; SEMI {Ireturn(None)}
;
instr_expr:
 | ac=acces; SET; e=expression{(ac,e)}
 | ap=appel; {IprocCall(ap)}
 | i=incr; ac=acces {Epreincr(i,ac) }
 | ac=acces; i=incr {Epostincr(i;ac)}
 | id=IDENT; i=incr {Epostincr(i,id)}
 | i=incr; id=IDENT {Epreincr(i,id) }
 | NEW; id=IDENT; OP; l=separated_list(COMMA,expression); CP {New(id,l)}
;
appel:
 | ac=acces; OP; le=list(expression) {(ac,le)}
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
 | id=IDENT; PT; id2=IDENT {(id,id2)}
; 
typ:
 | INT {TypInteger }
 | VOID {TypVoid } 
 | BOOL {TypBoolean}
 | STRING {TypClass "String"}
;
expression:
 | lit=literal { Literal(lit) }
 | id=IDENT {id}
 | NEQ; lit=literal {()}
 | MINUS; lit=literal {()}
 | e1=expression; bop=binop; e2=expression  { Binop(bop, e1, e2) }
 | e=expression; INSTOF; id=IDENT {e}
 | OP; id=IDENT; CP;e= expression {()}
 | OP; INT; CP; e=expression {Int e}
 | OP; BOOL; CP e=expression {Bool e}
 | ie=instr_expr {ie}
 | ac=acces {ac}
 | OP; e=expression; CP {e}

;
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
    |PUN {Pun}
    |MUN {Mun}
