open Amd64
open Ast

let gen_label =
  let count = ref 0 in
  fun () -> incr count;
    Printf.sprintf "__label__%05d" ! count

(* Le résultat de l'expression est stocké dans ras/eax *)
let rec compile_expr env e =
  match e.node with
    Econst Cint i -> movl ~$i ~%eax
  | Econst Cbool true -> movl ~$1 ~%eax
  | Econst Cbool false -> xorl ~%eax ~%eax
  | Enull -> xorl ~%rax ~%rax
  | Ebinop(e1, Bsub, e2) ->
     let code1 = compile_expr env e1 in
     let code2 = compile_expr env e2 in
     code1 ++
       pushq ~%rax ++
       code2++
       popq ~%r9 ++
       subl ~%r9d ~eax

      
  
let rec compile_block exit_lbl min_rpb cur_rbp env tbody =
  let amin_rbp, _, _, code =
  List.fold_left (fun (amin_rbp, acur_rbp, aenv, acode) i ->
    let nmin_rbp, ncur_rbp, nenv, icode = compile_instr exit_lbl amin_rbp acur_rbp aenv i
    in min amin_rbp nmin_rbp,
    ncur_rbp, nenv, acode ++ icode)
    (min_rbp, cur_rbp, env, nop) tbody
  in
  amin_rbp, cur_rbp, env, code

and compile_instr exit_lbl min_rbp cur_rbp env i =
  match i.node with
    Iskip ->
      min_rbp, cur_rbp, env, nop
  | Iblock bl -> compile_block exit_lbl min_rbp cur_rbp env bl
  | Iifelse (e, i1) ->
     let ecode = compile_expr env e in
     let min_rbp1, _, _, icode1 =
       compile_instr exit_lbl min_rbp cur_rbp env i1 in
     let min_rbp2, _, _, icode1 =
       compile_instr exit_lbl min_rbp cur_rbp env i2 in
     let lbl_else = gen_label() in
     let lbl_next = gen_label() in
     ecode ++
       cmpl ~$0 ~%eax ++
       je lbl_else ++
       icode1 ++
       jmplbl_next ++
       label lbl_else ++
       icode2 ++
       label lbl_next
in
min min_rbp1, min_rpb2, cur_rbp, env, ifcode
     failwith "todo"
     

let compile_prog (classes, (_,_,tbody)) =
  let _,_,_,code_main_body = (compile block "" 0 0 [] tbody)
  let code_main =
    glabel "main" ++
      pushq ~%rbp ++
      movq ~%rsp ~%rbp ++
      addq ~$min_rbp ~%rsp ++
      code_main_body ++
      label "__exit_main" ++
      (* call "_meth$main" ++
         movq ~:"hello_world_str" ~%rdi ++*)
      movq ~%rbp ~
      popq ~%rbp ++
      movq ~$0 ~%rax ++
      ret 
  in


  let code = nop in
  let data = nop
   (* label "hello_world_str" ++
      string "Hello, World !\n"*)
  in
  {
    text = code;
    data = data
  }
