open Ast
open Type_class

(** corriger du prog **)
type local_env = (ident * typ) list

let type_const c =
  match c with
  | Cint _ -> TypInteger
  | Cbool _ -> TypBoolean
  | Cstring _ -> TypClass "String"
  | Cnull -> TypNull
  
(* A completer binop *)
let type_binop t1 t2 op =
  match op with
  | Eq  | Neq                 -> if t1 == t2 then TypBoolean else failwith "Erreur de typage binop"
  | Mt  | Me | Lt | Le        -> if t1 == TypInteger && t2 == TypInteger then TypBoolean else failwith "Erreur de typage binop"
  | Add                       -> if t1 == TypClass "String" || t2 == TypClass "String" then TypClass "String"
    else if t1 == TypInteger && t1 == TypInteger then TypInteger
    else failwith "Erreur de typage binop"
  | Sub | Mult | Div | Modulo -> if t1 == TypInteger && t2 == TypInteger then TypInteger else failwith "Erreur de typage binop"
  | And | Or                  -> if t1 == TypBoolean && t2 == TypBoolean then TypBoolean else failwith "Erreur de typage binop"
  | _ -> failwith "unknow binop"

let type_unop t up =
  match up with
  |Neg -> if t == TypInteger then TypInteger else failwith "Erreur de typage unop"
  |Not -> if t == TypBoolean then TypBoolean else failwith "Erreur de typage unop"
      
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
  | Eunop (up, e) -> let te = type_expression env e in
		     let tr = type_unop te.info up in
		     {node = Eunop(up, te); info = tr}
  (*| EfunCall  ->
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
         
         failwith "todo mdr"
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
     if Type_class.subType te.info typ then 
       env, {node = Iset(ta, te); info = typ; }
     else
       failwith "Erreur de type affectation";
  | Iif (e, b) -> 
     let te = type_expression env e in
     let _, tb = type_block env b in
     env, {node = Iif(te, tb); info = TypVoid; }
  | Iifelse (e, b1, b2) ->
     let te = type_expression env e in
     let _, tb1 = type_block env b1 in
     let _, tb2 = type_block env b2 in
     env, {node = Iifelse(te, tb1, tb2); info = TypVoid; }
  | Ifor(oe1, oe2, oe3, b) ->
     let te1 = (match oe1 with
       | None -> None
       | Some e -> Some (type_expression env e)) in
     let te2 = (match oe2 with
       | None -> None
       | Some e -> Some (type_expression env e)) in
     let te3 = (match oe3 with
       | None -> None
       | Some e -> Some (type_expression env e)) in
     let _, tb = type_block env b in
     env, {node = Ifor(te1, te2, te3, tb); info = TypVoid; }
  (*  | IprocCall of 'info call *)
  |Idecl (typ, id, oe) ->
     begin
       match oe with
       |None -> (id, typ) :: env, {node = Idecl (typ, id, None);info = typ}
       |Some e -> let te = type_expression env e in
                  (id, typ) :: env, {node = Idecl (typ, id, Some te);info = typ}
     end
  | Iprint(oe) ->
     begin
       match oe with
       | None -> env, {node = Iprint(None); info = TypVoid;}
       | Some e -> let te = type_expression env e in
		   env, {node = Iprint(Some (te)); info = te.info}
     end
  | Ireturn (oe) ->
     begin
       match oe with
       |Some e -> let te = type_expression env e in
		  env, {node = Ireturn (Some (te)); info = te.info;}
       | None -> env, {node = Ireturn (None); info = TypVoid;}
  end
           
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
    
    