open Ast
(*type method_desc
  
  def ident
  oveeride
*)
type class_info = {
    class_name : ident;
    class_parent : ident;
    class_fields : (ident * (typ*ident)) list;
    class_methods: (ident * (typ* (typ * ident) list * ident)) list;
(*class_constr : (ident*method_desc) list;*)
    
}
  
let class_env : (ident, class_info) Hashtbl.t =
    Hashtbl.create 17

let rec extends c1 c2 =
  if (compare c1 "") == 0 then false
  else if  (compare c1 c2) == 0 then true
  else let par_c1 = (Hashtbl.find class_env c1).class_parent
      in extends par_c1 c2
let subType t1 t2 = 
  match t1, t2 with 
    TypBoolean, TypBoolean
  | TypInteger, TypInteger
  | TypVoid, TypVoid
  | TypNull, TypNull -> true
  | TypNull, TypClass _ -> true
  | TypClass c1, TypClass c2 -> extends c1 c2
  | _ -> false

let compatible t1 t2 = subType t1 t2 || subType t2 t1
     
let wf t = match t with 
    TypVoid | TypInteger | TypBoolean | TypClass "Object"
  | TypClass "String" -> true
  | TypClass a -> Hashtbl.mem class_env a
  | TypNull -> false
   
let topological_sort classes = classes (* à faire! *)
  
let init_class_env (classes : position class_def list) =
  (* check si string car sinon pas bien *)
  let () =
    Hashtbl.add class_env "Object" { class_name = "Object"; class_parent = "";
                                     class_fields = []; class_methods = [] } in
  let sorted_classes = topological_sort classes in
  List.iter (fun info ->
    let cls = info.name_def in
    let par = (match info.extends with
	Some p -> p
      | None -> "Object") in
    let decls = info.decls in
    
    let par_fields = (Hashtbl.find class_env par).class_fields in
    let par_meth = (Hashtbl.find class_env par).class_methods in
    let my_fields, my_methods =
      List.fold_left (
        fun (facc, macc) e ->
          match e with
            Att (typ, ident) ->
              (ident, (typ, cls)) :: facc, macc
          (* verifier que l'on a pas de doublons *)
          | Meth (typ, ident, params, _) ->
             facc, (ident, (typ, params, cls)) :: macc
          | _ -> (facc, macc) )
        ([], []) decls
  
    in
    Hashtbl.add class_env cls {class_name = cls;
                               class_parent = par;
                               class_fields = my_fields @ par_fields;
                               class_methods = my_methods @ par_meth} 
      
  ) sorted_classes
    
let select_field cls field =
  let class_desc = Hashtbl.find class_env cls in
  try
    Some (List.assoc field class_desc.class_fields)
  with Not_found -> None
    
let valid_signature targs tparams =
  try
    List.for_all2 (fun t (s , _) ->
      subType t s ) targs tparams
  with _ -> false
    
let select_method cls meth targs =
  let methods = (Hashtbl.find class_env cls).class_methods in
  let candidates =
    List.filter (fun (mname, (_, tparams, _)) ->
      meth = mname && valid_signature targs tparams) methods
  in
  match candidates with
    [] -> None
  | [l] -> Some l
  | _ -> failwith "Todo"

let for_all2 f l1 l2 =
  try
    List.for_all2 f l1 l2
  with Invalid_argument _ -> false

    
(*
strict subtype:
not (equal type t1 t2 && subtype t1 t2

*)
