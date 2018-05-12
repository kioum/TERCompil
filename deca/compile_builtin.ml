open Amd64
let builtins =
  comment "Primitives Assembleur" ++
    (*
      argument doit être dans rax/eax
    *)
    
    label "__builtin_print_int" ++
    movq ~:"__str_d" ~%rdi ++
    movq ~%rax ~%rsi ++
    xor ~%rax ~%rax ++
    call "printf" ++
    ret ++
    
    
    label "__builtin_print_boolean" ++
    cmpl ~$0 ~%eax ++
    je "__print_boolean_false" ++
    movq ~:"__str_true" ~%rdi ++
    jmp "__print_boolean_next" ++
    label "__print_boolean_false" ++
    movq ~:"__str_false" ~%rdi ++
    label "__print_boolean_next" ++
    call "puts" ++
    ret ++
    
    label "__builtin_print_String" ++
    movq ~:"__str_null" ~%rdi ++ (* place l'addresse de la chaîne 'null'
                                    dans rdi *)
    cmpq ~$0 ~%rax ++ (* teste si le String est le pointeur null *)
    je "__builtin_print_String_next" ++
    movq (addr ~ofs:8 ~%rax) ~%rdi ++ (* sinon charge l'adresse de la vraie chaine *)
    label "__builtin_print_String_next" ++
    call "puts" ++
    ret ++
    
    
    label "__builtin_null_error" ++
    movq ~:"__null_error_msg" ~%rdi ++
    call "puts" ++
    movq ~$1 ~%rdi ++
    call "exit" ++
    
    label "__builtin_div0_error" ++
    movq ~:"__div0_error_msg" ~%rdi ++
    call "puts" ++
    movq ~$1 ~%rdi ++
    call "exit"
    
let builtins_data =
  comment "Définitions des constantes" ++
    label "__str_true" ++
    string "true" ++
    label "__str_false" ++
    string "false" ++
    label "__str_null" ++
    string "null" ++
    label "__str_d" ++
    string "%d" ++
    label "__null_error_msg" ++
    string "null pointer exception\n" ++
    label "__div0_error_msg" ++
    string "division by 0 exception\n"
