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

let rec type_expression env e =
  match e.node with 
  | Econst c  -> { node = Econst c; info = type_const c; } 
  | Eaccess a -> let ta, typ = type_access env a in
                 { node = Eaccess(ta); info = typ; }
  | Ecast (typ, e) ->
     if not (wf typ) then failwith "error";
    let te = type_expression env e in
    if not (compatible typ te ) then failwith "error";
    {info = typ; node = Ecast typ te} 
 (* | Ebinop    ->
  | Eunop     ->
  | EfunCall  ->
  | EinstOf   ->
  | Enew      ->
  | Epreincr  ->
    | Epostincr ->*)
  | _         -> failwith "error"

and type_access env a =
  match a with
  | Aident id ->
     begin
       try
         let ta = List.assoc id env in
         Aident id, ta
       with Not_found ->
       (* select field *)
         failwith "todo"
     end
       
let rec type_instr env i =
  match i.node with
  | Iskip -> env,  {node = Iskip; info = TypVoid;}
  | Iblock b ->
     let _, tb = type_block env b in
     env, {node = Iblock(tb); info = TypVoid; }
  | Iset (a, e) ->
     let te = type_expression env e in
     let ta, typ = type_access env a in 
     env, {node = Iset(ta, te); info = TypVoid; }
  (*
  | Ifor   of 'info expression option * 'info expression option * 'info expression option * 'info block 
  | Iifelse    of 'info expression * 'info block * 'info block                           
  | Iif    of 'info expression * 'info block
    | IprocCall of 'info call *)
  |Idecl (typ, id) ->
      (id, typ) :: env, {node = Idecl (typ, id);info = typ}
       
  | Ireturn (Some e) ->
     let te = type_expression env e in
     env, {node = Ireturn (Some (te)); info = te.info;}
  | Ireturn (None) ->
     env, {node = Ireturn (None); info = TypVoid;}
           
and type_block env b =
  match b with
  | []   -> env, []
  | i::q ->
     let _,ti = type_instr env i in
     env, [ ti ]
       
let type_prog (classes, main) =
  (* verifier typage des classes TODO *)
  
  (*typage de main *)
  let _, _,body = main in
  let _, tbody = type_block [] body in
  tbody
    
    
