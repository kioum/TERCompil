open Ast

  (** corriger du prog **)
type local_env = (ident * typ) list

let type_const c =
  match c with
  | Cint _ -> TypInteger
  | Cbool _ -> TypBoolean
  | Cstring _ -> TypClass "String"
  
(* A completer binop *)
let type_binop = function
  | Add | Sub | Mult     -> TypInteger, TypInteger
  | Eq  | Neq | Lt  | Le -> TypInteger, TypBoolean
  | And | Or             -> TypBoolean, TypBoolean

let type_access env a =
  match a.node with
  | Aident id -> { node = Aident id; info = }
  
let type_expression env e =
  match e.node with 
  | Econst c  -> { node = Econst c; info = type_const c; } 
  | Eaccess   ->
  | Ebinop    ->
  | Eunop     ->
  | EfunCall  ->
  | EinstOf   ->
  | Enew      ->
  | Epreincr  ->
  | Epostincr ->
  | _         -> failwith "error"
     
let type_inst env i =
  match i.node with
  | Iskip -> env,  {node = Iskip; info = Tvoid;}
  | Iblock b ->
     let _, tb = type_block env b in
     env, {node = Iblock(tb); info = Tvoid; }
  | Iset a, e ->
     let te = type_expression env e in
     let ta = type_access env a in 
     env, {node = Iset(ta, te); info = Tvoid; }
  (*| Ifor   of 'info expression option * 'info expression option * 'info expression option * 'info block 
  | Iifelse    of 'info expression * 'info block * 'info block                           
  | Iif    of 'info expression * 'info block
    | IprocCall of 'info call *)
       
  | Ireturn (Some e) ->
     let te = type_expression env e in
     env, {node = Ireturn (Some (te)); info = te.info;}
  | Ireturn (None) =
         env, {node = Ireturn (None); info = Tvoid;)
           
let type_block env b =
  match b with
  | []   -> env, []
  | i::q ->
     let _,i = type_instr env i in
     env, [ ti ]
       
let type_prog (classes, main) =
       (* verifier typage des classes TODO *)
  
       (*typage de main *)
  let _, _,body = main in
  let _, tbody = type_block [] body in
  tbody
    
    
