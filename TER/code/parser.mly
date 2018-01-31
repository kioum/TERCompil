%{
(* let mk_loc p e = *)
  open Printf
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

%token CAST

%token IF ELSE
%token FOR
%token WHILE

%token SEMI

%token SET

%token VAR
%token INT BOOL STRING
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
%token VOID

%token EOF 


(* Déclaration des priorités*)


%right SET
%left OR
%left AND
%left EQUAL NEQ
%left LT LE MT ME INSTOF
%left PLUS MINUS
%left MULT DIV MOD
%right NOT PUN MUN NEG CAST
%left PT


(* SYMBOLE DE DÉPART DE LA GRAMMAIRE : ON DONNE SON NOM ET SON TYPE
   (QUI SERA LE TYPE DES PROGRAMMES DANS L'AST)

  IL FAUT REMPLACER 'unit' CI-DESSOUS PAR LE TYPE DE L'AST.
*)

%start prog
%type < unit > prog (* ? *)
    
%%

      (* RÈGLES DE GRAMMAIRE *)

prog:
   |cds=class_defs; mcd=main_class_def; EOF { (*cds@mcd*)() }
;
  
main_class_def:
 | PUBLIC; CLASS; id=IDENT; OA;
PUBLIC; STATIC; VOID; main=IDENT; OP; str=IDENT; arg=IDENT; OC; CC; CP;
bloc;CA {(*id*)(*(if main<>"main" || str <>"String" then $syntaxerror)*)()}
;
class_defs:
 | cds=list(class_def) {(*cds*)()}
;
class_def:
 | CLASS; id=IDENT; OA; dl=list(decl); CA; {(*id*)()}
 | CLASS; id=IDENT; EXTENDS; id2=IDENT; OA; dl=list(decl); CA; {(*(id;id2)*)()}
;
bloc:
 | OA; is=instructions; CA {(*is*)()}
;
instructions:
 | inst=list(instruction) {(*inst*)() }
;
instruction:
 | ie=instr_expr; SEMI {()}	   
 | id=IDENT; SET; e=expression; SEMI {(*Set(id,e)*)()}
 | id=IDENT; SEMI {()}
 | t=typ; id=IDENT; SEMI {()}
 | t=typ; id=IDENT; SET; e=expression; SEMI {()}
 | IF; OP; e=expression; CP; i=instruction; {()}
 | IF; OP; e=expression; CP; i1=instruction; ELSE; i2=instruction {()}
 | FOR; OP; e1=expression; COMMA; e2=expression; COMMA; e3=expression; CP; i=instruction {()}
 | bloc {()}
(* | id=IDENT; MUN; SEMI {()}
 | id=IDENT; PUN; SEMI {()}
 | datt=decl_att; SEMI {()}
 | dmet=decl_meth; SEMI {()}
 | dconst=decl_constr; SEMI {()}
	  | NEW; id=IDENT; OP; l=separated_list(COMMA,expression); CP; SEMI {(*New(id,l)*)()}*)
 | RETURN; e=expression; SEMI {()}
 | RETURN; SEMI {()}
;
instr_expr:
 | ac=acces; SET; e=expression{()}
 | ap=appel; {()}
 | MUN; ac=acces {()}
 | PUN; ac=acces {()}
 | ac=acces; MUN {()}
 | ac=acces; PUN {()}
 | id=IDENT; MUN {()}
 | id=IDENT; PUN {()}
 | MUN; id=IDENT {()}
 | PUN; id=IDENT {()}
 | NEW; id=IDENT; OP; l=separated_list(COMMA,expression); CP {(*New(id,l)*)()}
;
appel:
 | ac=acces; OP; le=list(expression) {()}
;
decl:
 | dc=decl_constr {()}
 | dm=decl_meth {()}
 | da=decl_att {()}
;
decl_constr:
 | id=IDENT; OP; dpars=separated_list(COMMA,param); CP; bloc {()}
;
decl_meth:
 | t=typ; id=IDENT; OP; dats=separated_list(COMMA,param); CP; bloc {()}
;
decl_att:
 | t=typ; id=IDENT; SEMI; {(*(id,t*)()}
 ;
param:
 | t=typ; id=IDENT {()}
;
acces:
 | id=IDENT; PT; id2=IDENT {()}
; 
typ:
 | INT {(*TypInteger*)()}
 (*| STRING {(*TypString*)()}*)
 | BOOL {(*TypBoolean*)()}
 | STRING {()}
 | VOID {()}
;
expression:
 | lit=literal { (*Literal(lit)*)()}
 | id=IDENT {()}
 | NEQ; lit=literal {()}
 | MINUS; lit=literal {()}
 | e1=expression; bop=binop; e2=expression  { (*Binop(bop, e1, e2)*)() }
 | e=expression; INSTOF; id=IDENT {()}
 | OP; id=IDENT; CP;e= expression {()}
 | OP; INT; CP; e=expression {()}
 | OP; BOOL; CP e=expression {()}
 | ie=instr_expr {()}
 | ac=acces {()}
 | OP; e=expression; CP {()}

;
literal:
 | i=CONST_INT {(*Int i*)()}
 | b=CONST_BOOL {(*bool b*)()}
 | s=CONST_STRING {()}
 | NULL {()}
;

%inline binop:
    | PLUS   { (*Add*) ()}
    | MINUS  { (*Sub*) ()}
    | MULT   { (*Mult*)()}
    | EQUAL  { (*Eq*)  ()}
    | NEQ    { (*Neq*) ()}
    | LT     { (*Lt *) ()}
    | LE     { (*Le *) ()}
    | MT     { (*Mt *) ()}
    | ME     { (*Me *) ()}
    | AND    { (*And*) ()}
    | OR     { (*Or*)  ()}
;
