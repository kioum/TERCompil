open Amd64
open Ast

let gen_label =
  let count = ref 0 in
  fun () -> incr count;
    Printf.sprintf "__label__%05d" ! count

(* Le résultat de l'expression est stocké dans ras/eax *)
let rec compile_expr env e =
  match e.node with
    Econst Cint i -> movl ~$(Int32.to_int i) ~%eax
  | Econst Cbool true -> movl ~$1 ~%eax
  | Econst Cbool false -> xorl ~%eax ~%eax
  | Econst Cnull -> xorq ~%rax ~%rax
  | Eaccess a ->  let ilc = compile_access env a in
                  ilc ++ movq (addr ~%rax) ~%rax
  | Ebinop(e1, op, e2) ->
     let code1 = compile_expr env e1 in
     let code2 = compile_expr env e2 in
     let lbl_next = gen_label () in
     match op with
     | Eq -> expr_calcul code1 code2 ++
        cmpl ~%r9d ~%eax
     (* sete %al ++ movzbl %al %eax *)
     (* movzbq %al %eax *)
     (*| Div ->   si b!= 0 :
       idiv %r10d  (*q->eax r -> edx *) cltd*)
     | And ->
        code1 ++
          cmpl  ~%eax ~$0 ++
          je lbl_next ++
          code2 ++
          label lbl_next
     | Or -> code1 ++
        cmpl ~$0 ~%eax ++
        jne lbl_next ++
        code2 ++
        label lbl_next
     | Sub -> expr_calcul code1 code2 ++
        subl ~%r9d ~%eax
     | Mult -> expr_calcul code1 code2 ++
        imull ~%r9d ~%eax
(* Ecall system.out.println token dans lexer.mll, chnager typeur, faire Ecall ... *)
and expr_calcul code1 code2 =
  code1 ++
    pushq ~%rax ++
    code2++
    popq ~%r9
          
and compile_access env a = assert false    
          
          
let rec compile_block exit_lbl min_rbp cur_rbp env tbody =
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
  | Idecl(_, id (*,oe *)) -> failwith "todo"
  (* match oe withœ
     |None -> (* init œdefault *)failwith "todo"
     |Some e -> compile_expr env e *)
  (*| Ireturn e ->
       match e with 
    |None -> min_rbp cur_rbp env nop++ jmp lbl_exit
    |Some e -> min_rbp cur_rbp env e_code++ jmp lbl_exit*)
  | Iifelse (e, i1, i2) ->
     let ecode = compile_expr env e in
     let min_rbp1, _, _, icode1 =
       compile_block exit_lbl min_rbp cur_rbp env i1 in
     let min_rbp2, _, _, icode2 =
       compile_block exit_lbl min_rbp cur_rbp env i2 in
     let lbl_else = gen_label() in
     let lbl_next = gen_label() in
     let ifcode =
       ecode ++
         cmpl ~$0 ~%eax ++
         je lbl_else ++
         icode1 ++
         jmp lbl_next ++
         label lbl_else ++
         icode2 ++
         label lbl_next
     in
     min min_rbp (min min_rbp1 min_rbp2), cur_rbp, env, ifcode
   | Iif (e, i1) ->
     let ecode = compile_expr env e in
     let min_rbp1, _, _, icode1 =
       compile_block exit_lbl min_rbp cur_rbp env i1 in
     let lbl_next = gen_label() in
     let ifcode =
       ecode ++
         cmpl ~$0 ~%eax ++
         je lbl_next ++
         icode1 ++
         label lbl_next
     in
     min min_rbp1 min_rbp, cur_rbp, env, ifcode
       

let compile_prog (classes, { instructions = tbody }) =
  let min_rbp,_,_,code_main_body = (compile_block "" 0 0 [] tbody) in
  let code_main =
    glabel "main" ++
      pushq ~%rbp ++
      movq ~%rsp ~%rbp ++
      addq ~$min_rbp ~%rsp ++
      code_main_body ++
      label "__exit_main" ++
      (* call "_meth$main" ++
         movq ~:"hello_world_str" ~%rdi ++*)
      movq ~%rbp ~%rsp ++
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
(* 
todo chaine
   créer structure de données pour les chaines (table_str)
   const strings parcourir Table_str et remplir la section data
   descripteur : commencer par desc$string
   Allocation de chaines
*)
