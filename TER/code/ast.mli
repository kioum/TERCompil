
(*module Symb_Tbl = Map.Make(String)
*)
type program={
  (string*class_info) list
}
and class_info = {
  extends: class_info;
(*attributs: (string*typ) list;
  functions: function_info Symb_Tbl.t;*)
}
