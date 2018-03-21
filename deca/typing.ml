open Ast

(** corriger du prog **)
type local_env = (ident * typ) list

let type_const c =
  match c with
  | Cint _ -> TypInteger
  | Cbool _ -> TypBoolean
  | Cstring _ -> TypClass "String"
  
(* A completer binop *)
let type_binop t1 t2 op =
  match op with
  | Eq  | Neq                 -> if t1 == t2 then TypBoolean else failwith "Erreur de typage"
  | Mt  | Me | Lt | Le        -> if t1 = TypInteger && t1 = TypInteger then TypBoolean else failwith "Erreur de typage"
  | Sub | Mult | Div | Modulo -> (*TypInteger, *) TypInteger
  | And | Or                  -> (*TypBoolean, *) TypBoolean
  | _ -> failwith "unknow binop"

let rec type_expression env e =
  match e.node with 
  | Econst c  -> { node = Econst c; info = type_const c; } 
  | Eaccess a -> let ta, typ = type_access env a in
                 { node = Eaccess(ta); info = typ; }
  | Ecast (typ, e) ->
     if not (Type_class.wf typ) then failwith "error";
    let te = type_expression env e in
    if not (Type_class.compatible typ te.info) then failwith "error";
    {info = typ; node = Ecast (typ, te)} 
  | Ebinop (e1, op, e2) ->
     let te1 = type_expression env e1 in
     let te2 = type_expression env e2 in
     let tr = type_binop te1.info te2.info op in (* Verifier que les types des deux cotés sont bons. *)
     {node = Ebinop(te1, op, te2); info = tr}
 (* | Eunop     ->
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
  | Afield (e,id) ->
     let te = type_expression env e in
     match te.info with
     | TypInteger  | TypBoolean | TypVoid | TypNull -> failwith "invalide acces"
     | TypClass cls ->
        begin
          match Type_class.select_field cls id with
          | Some ((tid, _)) ->
             (Afield (te, id), tid)
          | None -> failwith "Champs manquant"
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
  | Iif (e, b) -> 
     let te = type_expression env e in
     let _, tb = type_block env b in
     env, {node = Iif(te, tb); info = TypVoid; }
  | Iifelse (e, b1, b2) -> 
     let te = type_expression env e in
     let _, tb1 = type_block env b1 in
     let _, tb2 = type_block env b2 in
     env, {node = Iifelse(te, tb1, tb2); info = TypVoid; }
  (*| Ifor(e1, e2, e3, b) ->
     let te1 = type_expression env e1 in
     let te2 = type_expression env e2 in
     let te3 = type_expression env e3 in
     let _, tb = type_block env b in
    env, {node = ifor(te1, te2, te3, tb); info = TypVoid; }*)
  (*  | IprocCall of 'info call *)
  |Idecl (typ, id) ->
      (id, typ) :: env, {node = Idecl (typ, id);info = typ}  
  | Ireturn (Some e) ->
     let te = type_expression env e in
     env, {node = Ireturn (Some (te)); info = te.info;}
  | Ireturn (None) ->
     env, {node = Ireturn (None); info = TypVoid;}
           
and type_block env b =
  let _, trb =
    List.fold_left(fun (penv, li) i ->
      let nenv, ti = type_instr penv i in
      nenv, ti::li)(env,[]) b
  in
  env, List.rev trb
       
let type_prog (classes, main) =
  (* verifier typage des classes TODO *)
  Type_class.init_class_env classes;
  (*typage de main *)
  let _, tbody = type_block [] main.instructions in
  [], {name=main.name; params = main.params; instructions = tbody}
    
    
