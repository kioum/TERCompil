open Ast

type def_class = {
    name : ident;
    extends : ident;
    decls : (ident * (typ*ident)) list;
    methods: (ident * (typ * (ident) list * ident)) list;
    
}
  
let class_env : (ident, def_class) Hashtbl.t =
    Hashtbl.create 17

let rec extends c1 c2 =
  if c1 == "" then false
    if c1 == c2 then true
    else let par_c1 = (Hashtbl.find class_env c1).parent_name
       in extends par_c1 c2
       
let subType t1 t2 = 
  match t1, t2 with 
    TypBoolean, TypBoolean
  | TypInt, TypInt
  | TypVoid, TypVoid
  | TypNull, TypNull -> true
  | TypNull, TypClass _ -> true
  | TypClass c1, TClass c2 -> extends (c1, c2)
  | _ -> false

let wf t = match t with 
    TypVoid | Tint | TypBoolean | TypClass "Object"
  | TypClass "String" -> true
  | TypClass a -> Hashtbl.mem class_env a
  | TypNull -> false
     
     
let topological_sort classes = classes (* à faire! *)
  
let init_class_env (classes : position class_def list) =
  (* check si string car sinon pas bien *)
  let () =
    Hashtbl.add class_env "Object" { name = "Object"; extends = ""; decls = []; methods = [] } in
  let sorted_classes = topological_sort classes in
  List.iter (fun info ->
    let cls = info.name in
    let par = info.extends in
    let decls = info.decls in

    let par_fields = (Hashtbl.find class_env par).fields in
    let par_meth = (Hashtbl.find class_env par).methods in
    let my_fields, my_methods =
      List.fold_left (fun acc e ->
        match e with
          Dattr (typ, ident) ->
            (ident, (typ, cls)) :: facc, macc
        (* verifier que l'on a pas de doublons *)
        | Dmeth (typ, ident, params, _) ->
           facc, (ident, (typ, params, cls)) :: macc
        | _ -> acc )
        [] decls
    in
    
    Hashtbl.add class_env cls {name = cls; extends = par;
                               decls = my_fields @ par_fields;
                              methods = my_methods @ par_methods} 
      
  ) sorted_classes
    
let select_field cls field =
  let class_desc = Hashtbl.find class_env cls in
  try
    List.assoc field class_desc.fields
  with Not_found -> None
    
let valid_signature targs tparams =
  try
    List.for_all2 (fun t (s , _) ->
      subtype t s ) targs tparams
  with _ -> false
    
let select_method cls meth targs =
  let methods = (Hashtbl.find class_env cls).methods in
  let candidaes =
    List.filter (fun (mname, (_, tparams, _)) ->
      meth = mname && valid_signature args tparams) methods
  in
  match candidates with
    [] -> None
  | [l] -> Some l
  | _ -> failwith "todo"
