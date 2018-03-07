open Ast


let class_env : (ident , def_class) Hashtbl.t =
    Hashtbl.create 17

let rec extends c1 c2 =
    if c1 = "" then false
    if c1 = c2 then true
    else let par_c1 = (Hashtbl.find class_env c1).parent_name
          in extends par_c1 c2

let subType t1 t2 = 
    match t1, t2 with 
        TypBoolean, TypBoolean
        | TypInt, TypInt
        | TypVoid, TypVoid
        | TypNull, TypNull -> true
        | TypNull, TypClass _ -> true
        | TypClass c1, TClass c2 ->
            extends c1, c2
        | _ -> false

type def_class = {
    name : ident;
    extends : ident;
    decls : (ident * (typ*ident)) list;
}

let wf t =  match t with 
    TypVoid | Tint | TypBoolean | TypClass "Object"
    | TypClass "String" -> true
    | TypClass a -> Hashtbl.mem class_env a
    | TypNull -> false
