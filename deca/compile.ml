open Amd64
open Ast
open Compile_builtin
open Printf

let gen_label =
  let count = ref 0 in
  fun () -> incr count;
    Printf.sprintf "__label__%05d" ! count

let str_table = Hashtbl.create 17
	   
(* Le résultat de l'expression est stocké dans ras/eax *)
let rec compile_expr env e =
  match e.node with
    Econst Cint i -> movl ~$(Int32.to_int i) ~%eax
  | Econst Cstring str ->
     let label =
     try
       Hashtbl.find str_table str
     with Not_found ->
       let new_label = "__str" ^ (gen_label()) in
       Hashtbl.add str_table str new_label;
       new_label
in
movq ~$24 ~%rdi ++
  call "malloc" ++
  movq ~$1234 (addr ~%rax) ++
  movq ~:label (addr ~ofs:8 ~%rax)++
  movq ~$(String.length str) (addr ~ofs:16 ~%rax)

  | Econst Cbool true -> movl ~$1 ~%eax
  | Econst Cbool false -> xorl ~%eax ~%eax
  | Econst Cnull -> xorq ~%rax ~%rax
  | Eaccess a -> let pos_rbp = compile_access env a in
		 movq (addr ~ofs:pos_rbp ~%rbp) ~%rax
  | Eunop(up, e) ->
     begin
       let ecode = compile_expr env e in
       let lbl_false = gen_label () in
       let lbl_true = gen_label () in
       match up with
       | Neg -> ecode ++
          negl ~%eax
	  
       | Not ->  ecode ++
	  cmpl ~$0 ~%eax ++
	  jne lbl_false ++
	  movl ~$1 ~%eax ++
	  jmp lbl_true ++
	  label lbl_false ++
	  xorl ~%eax ~%eax ++
	  label lbl_true
     end
  | Ebinop(e1, op, e2) ->
     let code1 = compile_expr env e1 in
     let code2 = compile_expr env e2 in
     let lbl_next = gen_label () in
     match op with
     | Eq -> expr_calcul code1 code2 ++
        cmpl ~%eax ~%r9d ++
	je lbl_next ++
	movl ~$0 ~%eax ++
	label lbl_next 
	       
	
     (* sete %al ++ movzbl %al %eax *)
     (* movzbq %al %eax *)
     (*| Div ->   si b!= 0 :
       idiv %r10d  (*q->eax r -> edx *) cltd*)
     | And ->
        code1 ++
          cmpl ~$0 ~%eax ++
          je lbl_next ++
          code2 ++
          label lbl_next
     | Or -> code1 ++
        cmpl ~$0 ~%eax ++
        jne lbl_next ++
        code2 ++
        label lbl_next
     | Add ->
	begin
	  if(e1.info == TypClass "String" || e2.info == TypClass "String") then
	    failwith "todo toString"
	  else
	    expr_calcul code1 code2 ++
	      addl ~%r9d ~%eax
	end
     | Sub -> expr_calcul code1 code2 ++
        subl ~%eax ~%r9d
     | Mult -> expr_calcul code1 code2 ++
        imull ~%r9d ~%eax
	
and expr_calcul code1 code2 =
  code1 ++
    pushq ~%rax ++
    code2++
    popq ~%r9
          
and compile_access env a =
  match a with
  | Aident id -> List.assoc id env
  | Afield (e, id) -> failwith "todo field"
  | _ -> failwith "Error access compile"
     
          
let rec compile_block exit_lbl min_rbp cur_rbp env tbody =
  let amin_rbp, _, _, code =
    List.fold_left (fun (amin_rbp, acur_rbp, aenv, acode) i ->
      let nmin_rbp, ncur_rbp, nenv, icode =
	compile_instr exit_lbl amin_rbp acur_rbp aenv i
      in
      min amin_rbp nmin_rbp,
      ncur_rbp,
      nenv,
      acode ++ icode) (min_rbp, cur_rbp, env, nop) tbody
  in
  amin_rbp, cur_rbp, env, code
    
and compile_instr exit_lbl min_rbp cur_rbp env i =
  match i.node with
    Iskip ->
      min_rbp, cur_rbp, env, nop
  | Iblock bl -> compile_block exit_lbl min_rbp cur_rbp env bl
  | Idecl(_, id, oe) ->
     begin
       match oe with
       (*init default*)
       |None -> min_rbp, cur_rbp-8, (id, cur_rbp-8)::env, nop
       |Some e ->
	  let ecode = compile_expr env e in
	  let declcode =
	    ecode ++
	      movq ~%rax (addr ~ofs:(cur_rbp-8) ~%rbp)
	  in
	  min_rbp, cur_rbp-8, (id, cur_rbp-8)::env, declcode
     end
  |Iset(a, e) -> let ecode = compile_expr env e in
		 let pos_rbp = compile_access env a in
		 let setcode = ecode ++
		   movq ~%rax (addr ~ofs:(pos_rbp) ~%rbp)
		 in
		 min min_rbp pos_rbp, cur_rbp, env, setcode 
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
   | Iprint (oe) ->
      begin
	match oe with
	| None -> let printcode = ret
		  in
		  min_rbp, cur_rbp, env, printcode
	| Some e -> let ecode = compile_expr env e in
		    let printcode =
		      ecode ++
			begin 
			  match e.info with
			  |TypInteger -> call "__builtin_print_int"
			  |TypBoolean -> call "__builtin_print_boolean"
			  |TypClass "String" -> call "__builtin_print_String"
			  |_ -> nop
			end
		    in min_rbp, cur_rbp, env, printcode 
      end
      

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


  let code = code_main ++ 
    builtins
  in
  let data = nop ++
    (Hashtbl.fold (fun str lbl a_code -> a_code ++ label lbl ++ string str) str_table nop) ++
    builtins_data
    
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