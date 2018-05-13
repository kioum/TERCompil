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
  
(* binop *)
let type_binop t1 t2 op =
  match op with
  | Eq  | Neq                 -> if t1 == t2 then TypBoolean else failwith "Erreur de typage binop"
  | Mt  | Me | Lt | Le        -> if t1 == TypInteger && t2 == TypInteger then TypBoolean else failwith "Erreur de typage binop"
  | Add                       -> if t1 == TypClass "String" || t2 == TypClass "String" then TypClass "String"
    else if t1 == TypInteger && t1 == TypInteger then TypInteger
    else failwith "Erreur de typage binop"
  | Sub | Mult | Div | Modulo -> if t1 == TypInteger && t2 == TypInteger then TypInteger else failwith "Erreur de typage binop"
  | And | Or                  -> if t1 == TypBoolean && t2 == TypBoolean then TypBoolean else failwith "Erreur de typage binop"

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
     if not (Type_class.wf typ) then failwith "Error type inconnu Cast";
    let te = type_expression env e in
    if not (Type_class.compatible typ te.info) then failwith "Error type pas compatible Cast";
    {info = typ; node = Ecast (typ, te)} 
  | Ebinop (e1, op, e2) ->
     let te1 = type_expression env e1 in
     let te2 = type_expression env e2 in
     let tr = type_binop te1.info te2.info op in (* Verifier que les types des deux cotés sont bons. *)
     {node = Ebinop(te1, op, te2); info = tr}
  | Eunop (up, e) -> let te = type_expression env e in
		     let tr = type_unop te.info up in
		     {node = Eunop(up, te); info = tr}
  | Epreincr(incr, a) -> let ta, typ = type_access env a in
			 if(typ == TypInteger) then 
			   {node = Epreincr(incr, ta); info = typ}
			 else failwith "Erreur Type ++pre"
  | Epostincr(a, incr) -> let ta, typ = type_access env a in
			  if(typ == TypInteger) then 
			    {node = Epostincr(ta, incr); info = typ}
			  else failwith "Erreur Type post++"
  | Eset (a, e) ->
     let te = type_expression env e in
     let ta, typ = type_access env a in
     if Type_class.subType te.info typ then 
       {node = Eset(ta, te); info = TypVoid; }
     else
       failwith "Erreur de type affectation";
  | EfunCall (c) -> let (a , le) = c in
		    let (ta, typ) = type_access env a in
		    let lte = List.fold_left(fun li e ->
		      let te = type_expression env e in te::li) [] le in 
		    {node = EfunCall((ta, lte)); info = TypVoid}
		      
  | EinstOf(e, id) -> let te = type_expression env e in
		      {node = EinstOf(te, id); info = te.info}
  | Enew(id, le) -> let lte = List.fold_left(fun li e ->
    let te = type_expression env e in te::li) [] le in 
		    {node = Enew(id, lte); info = TypClass id}
     
and type_access env a =
  match a with
  | Aident id ->
     begin
       try
         let ta = List.assoc id env in
         Aident id, ta
       with Not_found ->
	 (* select field *)
	 failwith "todo recuperer field"
     end
  | Afield (e,id) ->
     let te = type_expression env e in
     match te.info with
     | TypInteger  | TypBoolean | TypVoid | TypNull -> failwith "invalide access"
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
  | Iif (e, b) -> 
     let te = type_expression env e in
      if not (Type_class.compatible TypBoolean te.info) then failwith "error if";
     let _, tb = type_block env b in
     env, {node = Iif(te, tb); info = TypVoid; }
  | Iifelse (e, b1, b2) ->
     let te = type_expression env e in
      if not (Type_class.compatible TypBoolean  te.info) then failwith "error if else";
     let _, tb1 = type_block env b1 in
     let _, tb2 = type_block env b2 in
     env, {node = Iifelse(te, tb1, tb2); info = TypVoid; }
  | Ifor(oe1, oe2, oe3, b) ->
     let te1 = (match oe1 with
       | None -> None
       | Some e -> let te = (type_expression env e) in
		   if (te.info != TypVoid) then
		     failwith "Error Typage for"
		   else Some te) in
     let te2 = (match oe2 with
       | None -> None
       | Some e ->  let te = (type_expression env e) in
		    if (te.info != TypBoolean) then
		      failwith "Error Typage for"
		    else Some te) in
     let te3 = (match oe3 with
       | None -> None
       | Some e ->  let te = (type_expression env e) in
		    if (te.info != TypVoid && te.info != TypInteger) then
		      failwith "Error Typage for"
		    else Some te) in
     let _, tb = type_block env b in
     env, {node = Ifor(te1, te2, te3, tb); info = TypVoid; }
  | Iexpr e ->
     env, { node = Iexpr (type_expression env e); info = TypVoid; }
  |Idecl (typ, id, oe) ->
     begin
       match oe with
       |None -> (id, typ) :: env, {node = Idecl (typ, id, None); info = typ}
       |Some e -> let te = type_expression env e in
		  if not (Type_class.compatible typ te.info) then failwith "error decl";
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
    
    
