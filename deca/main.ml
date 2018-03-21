(* Programme principal *)


let ext = ".java"
let usage = Format.sprintf "usage: %s [options] file%s" Sys.argv.(0) ext

let parse_only = ref false

let print_pos lb=
  let open Lexing in
  let p1 = Lexing.lexeme_start_p lb in
  let p2 = Lexing.lexeme_end_p lb in
  Printf.eprintf "Ligne %d, car %d, à ligne %d, cr %d\n%!" p1.pos_lnum(p1.pos_cnum - p1.pos_bol) p2.pos_lnum(p1.pos_cnum - p1.pos_bol)

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
    let p = Parser.prog Lexer.token lb in
    if !parse_only then exit 0;
    let t = Typing.type_prog p in
    let code = Compile.compile_prog t in
    close_in c;
    let ofile = (Filename.chop_suffix file ext) ^ ".s" in
    Amd64.print_in_file~file:ofile code
  with
  | Lexer.Lexical_error s -> print_pos lb; Format.eprintf "Lexical error\n%!";
      exit 1
  | Parser.Error -> print_pos lb; Format.eprintf "Syntax error\n%!";
      exit 1
  | e -> Format.eprintf "Anomaly\n";
                       exit 2
