
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | VOID
  | VAR
  | THIS
  | STATIC
  | SET
  | SEMI
  | RETURN
  | PUN
  | PUBLIC
  | PT
  | PRINT
  | PLUS
  | OR
  | OP
  | OC
  | OA
  | NULL
  | NOT
  | NEW
  | NEQ
  | MUN
  | MULT
  | MT
  | MOD
  | MINUS
  | ME
  | LT
  | LE
  | INT
  | INSTOF
  | IF
  | IDENT of (string)
  | FOR
  | EXTENDS
  | EQUAL
  | EOF
  | ELSE
  | DIV
  | CP
  | CONST_STRING of (string)
  | CONST_INT of (int32)
  | CONST_BOOL of (bool)
  | COMMA
  | CLASS
  | CC
  | CAST
  | CA
  | BOOL
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState179
  | MenhirState174
  | MenhirState155
  | MenhirState151
  | MenhirState144
  | MenhirState142
  | MenhirState136
  | MenhirState131
  | MenhirState122
  | MenhirState121
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState112
  | MenhirState108
  | MenhirState107
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState100
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState94
  | MenhirState93
  | MenhirState91
  | MenhirState90
  | MenhirState87
  | MenhirState83
  | MenhirState81
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState38
  | MenhirState35
  | MenhirState30
  | MenhirState29
  | MenhirState25
  | MenhirState20
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState13
  | MenhirState7
  | MenhirState3
  | MenhirState0
  
(* let mk_loc p e = *)
  open Printf

let rec _menhir_goto_list_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let le = _v in
        let (_menhir_stack, _menhir_s, ac) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                                      (()) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ap = _v in
        let _v : (unit) =              (()) in
        _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_expression_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
        let _2 = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let id = _v in
        let ((_menhir_stack, _menhir_s, e), _) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                                   (()) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, ac) = _menhir_stack in
    let _2 = () in
    let _v : (unit) =                  (()) in
    _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | AND | COMMA | CP | EQUAL | INSTOF | LE | LT | ME | MT | MULT | OR | PLUS | SEMI ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, ac) = _menhir_stack in
    let _2 = () in
    let _v : (unit) =                  (()) in
    _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s lit ->
    let _v : (unit) =                ( (*Literal(lit)*)()) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState48 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (unit list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MINUS | MT | MUN | NEQ | NEW | NULL | OP | OR | PLUS | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Add*) ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
        let _10 = () in
        let _v : (unit) = let bop =
          let _1 = _10 in
                       ( (*Mult*)())
        in
                                                    ( (*Binop(bop, e1, e2)*)() ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | IDENT _ | MUN | NEW | NULL | OP | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, ac), _, e) = _menhir_stack in
            let _2 = () in
            let _v : (unit) =                               (()) in
            _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | IDENT _ | MUN | NEW | NULL | OP | OR | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Or*)  ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState81 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | MUN | NEQ | NEW | NULL | OP | OR | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Neq*) ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MT | MUN | NEQ | NEW | NULL | OP | OR | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Mt *) ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState83 | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MINUS | MT | MUN | NEQ | NEW | NULL | OP | OR | PLUS | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Sub*) ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MT | MUN | NEQ | NEW | NULL | OP | OR | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Me *) ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MT | MUN | NEQ | NEW | NULL | OP | OR | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Lt *) ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MT | MUN | NEQ | NEW | NULL | OP | OR | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Le *) ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | MUN | NEQ | NEW | NULL | OP | OR | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*Eq*)  ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | IDENT _ | MUN | NEW | NULL | OP | OR | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let bop =
              let _1 = _10 in
                           ( (*And*) ())
            in
                                                        ( (*Binop(bop, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState80 | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CONST_BOOL _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | CONST_STRING _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState80 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MUN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState80 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | NEW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NULL ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | OP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PUN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | COMMA | CP | SEMI ->
            _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | IDENT _ | MUN | NEW | NULL | OP | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, e) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit) =                              (()) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | IDENT _ | MUN | NEW | NULL | OP | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, id), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                   (()) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | IDENT _ | MUN | NEW | NULL | OP | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, e) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit) =                              (()) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState98 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                         (()) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState100 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                               (()) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState104 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | FOR ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | IDENT _v ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | IF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | OA ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | RETURN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState108 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, id), _, e) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (unit) =                                      ((*Set(id,e)*)()) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState113 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState115 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState117 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | FOR ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | IDENT _v ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | IF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | OA ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | RETURN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | INSTOF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LE ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | ME ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | NEQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | OR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState122 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, t), id), _, e) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (unit) =                                             (()) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let inst = _v in
        let _v : (unit) =                           ((*inst*)() ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, is) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                            ((*is*)()) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState17 | MenhirState136 | MenhirState105 | MenhirState131 | MenhirState118 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
                let _v : (unit) =         (()) in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | MenhirState16 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, id), _, xs0), _, _5) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (unit) = let dpars =
                  let xs = xs0 in
                      ( xs )
                in
                                                                             (()) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let dc = _v in
                let _v : (unit) =                   (()) in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | MenhirState144 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s, t), id), _, xs0), _, _6) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _v : (unit) = let dats =
                  let xs = xs0 in
                      ( xs )
                in
                                                                                   (()) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let dm = _v in
                let _v : (unit) =                 (()) in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | MenhirState174 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, id), main), str), arg), _, _15) = _menhir_stack in
                    let _16 = () in
                    let _14 = () in
                    let _13 = () in
                    let _12 = () in
                    let _9 = () in
                    let _7 = () in
                    let _6 = () in
                    let _5 = () in
                    let _4 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : (unit) =         ((*id*)(*(if main<>"main" || str <>"String" then $syntaxerror)*)()) in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EOF ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, cds), mcd) = _menhir_stack in
                        let _3 = () in
                        let _v : ( unit ) =                                             ( (*cds@mcd*)() ) in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _1 = _v in
                        Obj.magic _1
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), id), _, xs0) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (unit) = let l =
          let xs = xs0 in
              ( xs )
        in
                                                                     ((*New(id,l)*)()) in
        _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_instr_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState121 | MenhirState116 | MenhirState114 | MenhirState112 | MenhirState107 | MenhirState103 | MenhirState18 | MenhirState25 | MenhirState96 | MenhirState93 | MenhirState90 | MenhirState87 | MenhirState80 | MenhirState83 | MenhirState81 | MenhirState78 | MenhirState75 | MenhirState73 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState51 | MenhirState49 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, ie) = _menhir_stack in
        let _v : (unit) =                  (()) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState17 | MenhirState136 | MenhirState105 | MenhirState131 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, ie) = _menhir_stack in
            let _2 = () in
            let _v : (unit) =                        (()) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _v : (unit) =             (()) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _2 = () in
    let _v : (unit) =                  (()) in
    _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let id2 = _v in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                            (()) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState20 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, ac) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                  (()) in
            _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
        | MenhirState35 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, ac) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                  (()) in
            _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
        | MenhirState103 | MenhirState112 | MenhirState114 | MenhirState116 | MenhirState121 | MenhirState107 | MenhirState18 | MenhirState25 | MenhirState96 | MenhirState93 | MenhirState90 | MenhirState29 | MenhirState87 | MenhirState49 | MenhirState78 | MenhirState80 | MenhirState83 | MenhirState81 | MenhirState55 | MenhirState57 | MenhirState75 | MenhirState73 | MenhirState59 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState61 | MenhirState63 | MenhirState51 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUN ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
            | OP ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
            | PUN ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
            | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MINUS | MT | MULT | NEQ | NEW | NULL | OR | PLUS | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, ac) = _menhir_stack in
                let _v : (unit) =             (()) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState17 | MenhirState136 | MenhirState105 | MenhirState131 | MenhirState118 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUN ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
            | OP ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
            | PUN ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _2 = () in
    let _v : (unit) =                  (()) in
    _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let lit = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (unit) =                     (()) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let lit = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (unit) =                       (()) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState121 | MenhirState116 | MenhirState114 | MenhirState112 | MenhirState107 | MenhirState103 | MenhirState18 | MenhirState25 | MenhirState96 | MenhirState93 | MenhirState90 | MenhirState87 | MenhirState80 | MenhirState78 | MenhirState75 | MenhirState73 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState51 | MenhirState49 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState18 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (unit) =                 (()) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST_BOOL _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | CONST_STRING _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MUN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NULL ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | OP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PUN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MUN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
    | PT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
    | PUN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                   (()) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | SET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST_BOOL _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | CONST_STRING _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MUN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NEW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NULL ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | OP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PUN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST_BOOL _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | CONST_STRING _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | MUN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | NEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | NEW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | NULL ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | OP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | PUN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_class_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | PUBLIC ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179

and _menhir_goto_loption_separated_nonempty_list_COMMA_param__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | OA ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | OA ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | CA ->
        _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState20 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MINUS | MT | MULT | MUN | NEQ | NEW | NULL | OP | OR | PLUS | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, id) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                  (()) in
            _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState25 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState25 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
        | MUN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | PT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | PUN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | INSTOF | LE | LT | ME | MINUS | MT | MULT | NEQ | OR | PLUS ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | INT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState25 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NEQ ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | OP ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =         (()) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NEQ ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NULL ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | OP ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState29 in
                let _v : (unit list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState35 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MINUS | MT | MULT | MUN | NEQ | NEW | NULL | OP | OR | PLUS | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, id) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                  (()) in
            _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | NULL ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MUN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
    | PT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
    | PUN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MINUS | MT | MULT | NEQ | NEW | NULL | OP | OR | PLUS | SEMI ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    let _v : (unit) =                   (()) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int32) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (unit) =                ((*Int i*)()) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let b = _v in
    let _v : (unit) =                 ((*bool b*)()) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((((_menhir_stack, _menhir_s), _, e1), _), _, e2), _), _, e3), _), _, i) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (unit) =                                                                                          (()) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | FOR ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | IDENT _v ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
            | IF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | MUN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | NEW ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | OA ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | PUN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | RETURN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
        | BOOL | CA | FOR | IDENT _ | IF | INT | MUN | NEW | OA | PUN | RETURN | VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, e), _), _, i) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit) =                                             (()) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, e), _), _, i1), _, i2) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (unit) =                                                                   (()) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState136 | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | FOR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | IDENT _v ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | IF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MUN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | NEW ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | OA ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | PUN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | RETURN ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | VOID ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | CA ->
            _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState142 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | FOR ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | IDENT _v ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MUN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NEW ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | OA ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | PUN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | RETURN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | CA ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), id), _, dl) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                            ((*id*)()) in
            _menhir_goto_class_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), id), id2), _, dl) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                                                ((*(id;id2)*)()) in
            _menhir_goto_class_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState142 | MenhirState13 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let (_menhir_stack, _menhir_s, t) = _menhir_stack in
            let _v : (unit) =                    (()) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | INT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | VOID ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
            | CP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : (unit list) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 | MenhirState17 | MenhirState131 | MenhirState105 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, t), id) = _menhir_stack in
                let _3 = () in
                let _v : (unit) =                          (()) in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | SET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CONST_BOOL _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | CONST_INT _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | CONST_STRING _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | IDENT _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | MINUS ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | MUN ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | NEQ ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | NEW ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | NULL ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | OP ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | PUN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 | MenhirState151 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | OP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | INT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | VOID ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | CP ->
                    _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, t), id) = _menhir_stack in
                let _3 = () in
                let _v : (unit) =                           ((*(id,t*)()) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let da = _v in
                let _v : (unit) =                (()) in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_class_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let cds = _v in
        let _v : (unit) =                        ((*cds*)()) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PUBLIC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | OA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | PUBLIC ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | STATIC ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | VOID ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | IDENT _v ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_stack = (_menhir_stack, _v) in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | OP ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | IDENT _v ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_stack = (_menhir_stack, _v) in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _tok = _menhir_env._menhir_token in
                                                (match _tok with
                                                | IDENT _v ->
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    let _menhir_stack = (_menhir_stack, _v) in
                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                    let _tok = _menhir_env._menhir_token in
                                                    (match _tok with
                                                    | OC ->
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                        let _tok = _menhir_env._menhir_token in
                                                        (match _tok with
                                                        | CC ->
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                            let _tok = _menhir_env._menhir_token in
                                                            (match _tok with
                                                            | CP ->
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let _menhir_env = _menhir_discard _menhir_env in
                                                                let _tok = _menhir_env._menhir_token in
                                                                (match _tok with
                                                                | OA ->
                                                                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                                                | _ ->
                                                                    assert (not _menhir_env._menhir_error);
                                                                    _menhir_env._menhir_error <- true;
                                                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
                                                            | _ ->
                                                                assert (not _menhir_env._menhir_error);
                                                                _menhir_env._menhir_error <- true;
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                raise _eRR)
                                                        | _ ->
                                                            assert (not _menhir_env._menhir_error);
                                                            _menhir_env._menhir_error <- true;
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            raise _eRR)
                                                    | _ ->
                                                        assert (not _menhir_env._menhir_error);
                                                        _menhir_env._menhir_error <- true;
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        raise _eRR)
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    raise _eRR)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            raise _eRR)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        raise _eRR)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =         (()) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =        ((*TypInteger*)()) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | VOID ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | CP ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =         ((*TypBoolean*)()) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EXTENDS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | OA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOL ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | IDENT _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                    | INT ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | VOID ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | CA ->
                        _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | OA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | CA ->
                _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ( unit ) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PUBLIC ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

