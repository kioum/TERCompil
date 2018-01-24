%{
(* let mk_loc p e = *)
%}
  
(* Déclaration des tokens *)
  
%token <int> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT

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
%token INT BOOL
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
bloc;CA {(*id*)(if main<>"main" || str <>"String" then $syntaxerror);()}
;
class_defs:
 | cds=list(class_def) {(*cds*)()}
;
class_def:
 | CLASS; id=IDENT; bloc; {(*id*)()}
 | CLASS; id=IDENT; EXTENDS; id2=IDENT; bloc; {(*(id;id2)*)()}
;
bloc:
 | OA; is=instructions; CA {(*is*)()}
;
instructions:
 | inst=list(instruction) {(*inst*)() }
;
instruction:
 | l=location; SET; e=expression; SEMI {(*Set(l,e)*)()}
 | id=IDENT; MUN; SEMI {()}
 | id=IDENT; PUN; SEMI {()}
 | datt=decl_att; SEMI {()}
 | NEW; id=IDENT; OP; l=separated_list(COMMA,expression); CP; SEMI {(*New(id,l)*)()}
;
decl_att:
 | t=typ; id=IDENT {(*(id,t*)()}
;
typ:
 | INT {(*TypInteger*)()}
 (*| STRING {(*TypString*)()}*)
 | BOOL {(*TypBoolean*)()}
;
expression:
 | loc=location {(*Literal(lit)*)()}
 | lit=literal { (*Literal(lit)*)()}
 | e1=expression; bop=binop; e2=expression  { (*Binop(bop, e1, e2)*)() }
;
literal:
 | i=CONST_INT {(*Int i*)()}
;
location:
 | id=IDENT {(*identifier(id)*)()}
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
