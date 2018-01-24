%{

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


(*
%right SET
%left OR
%left AND
%left EQUAL NEQ
%left LT LE MT ME INSTOF
%left PLUS MINUS
%left MULT DIV MOD
%right NOT PUN MUN NEG CAST
%left PT
*)

/* SYMBOLE DE DÉPART DE LA GRAMMAIRE : ON DONNE SON NOM ET SON TYPE
   (QUI SERA LE TYPE DES PROGRAMMES DANS L'AST)

  IL FAUT REMPLACER 'unit' CI-DESSOUS PAR LE TYPE DE L'AST.
*/
   
%start prog
%type < unit > prog (* ? *)
    
%%

      (* RÈGLES DE GRAMMAIRE *)

prog:
   |cds=class_decls; mcd=main_class_decl; EOF { (*cds@mcd*)() }
;
  
main_class_decl:
  PUBLIC; CLASS; id=IDENT; OA;
PUBLIC; STATIC; VOID; main=IDENT; OP; str=IDENT; arg=IDENT; OC; CC; CP; (* bloc*) CA {(*id*)(if main<>"main" || str <>"String" then $syntaxerror);()}
;
class_decls:
  cds=list(class_decl) {(*cds*)()}
;
class_decl:
 |CLASS; id=IDENT; OA; (* bloc *) CA {(*id*)()}
 |CLASS; id=IDENT; EXTENDS; id2=IDENT; OA; (* bloc *) CA {(*(id;id2)*)()}
;
