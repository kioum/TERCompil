(* Programme principal *)


let ext = ".java"
let usage = Format.sprintf "usage: %s [options] file%s" Sys.argv.(0) ext

let parse_only = ref false


let spec =
  ["--parse-only", Arg.Set parse_only, "  stops after parsing";
]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ext) then
      raise (Arg.Bad "invalid extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let _p = Parser.prog Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
  with
  | Lexer.Lexical_error s -> Format.eprintf "Lexical error";
      exit 1
  | Parsing.Parse_error -> Format.eprintf "Syntax error";
      exit 1
  | e -> Format.eprintf "Anomaly";
                       exit 2
