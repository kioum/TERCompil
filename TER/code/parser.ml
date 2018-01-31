
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | VOID
  | VAR
  | THIS
  | STRING
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
  | MenhirState180
  | MenhirState175
  | MenhirState156
  | MenhirState152
  | MenhirState145
  | MenhirState143
  | MenhirState137
  | MenhirState132
  | MenhirState123
  | MenhirState122
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState109
  | MenhirState108
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState101
  | MenhirState99
  | MenhirState98
  | MenhirState97
  | MenhirState95
  | MenhirState94
  | MenhirState92
  | MenhirState91
  | MenhirState88
  | MenhirState84
  | MenhirState82
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState71
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
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState39
  | MenhirState36
  | MenhirState31
  | MenhirState30
  | MenhirState26
  | MenhirState21
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState14
  | MenhirState8
  | MenhirState3
  | MenhirState0
  
(* let mk_loc p e = *)
  open Printf

let rec _menhir_goto_list_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState79 ->
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
    | MenhirState81 ->
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
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
        let _2 = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
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

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, ac) = _menhir_stack in
    let _2 = () in
    let _v : (unit) =                  (()) in
    _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | AND | COMMA | CP | EQUAL | INSTOF | LE | LT | ME | MT | MULT | OR | PLUS | SEMI ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
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
    | MenhirState88 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState49 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (unit list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
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
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | IDENT _ | MUN | NEW | NULL | OP | PUN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, ac), _, e) = _menhir_stack in
            let _2 = () in
            let _v : (unit) =                               (()) in
            _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState82 | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState84 | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState81 | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | CONST_BOOL _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | CONST_INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | CONST_STRING _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | IDENT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState81 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MUN ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState81 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
        | NEW ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NULL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PUN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | COMMA | CP | SEMI ->
            _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState98
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState99 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                         (()) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState101 in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState105 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | FOR ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | IDENT _v ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | OA ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | RETURN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | STRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState109 in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState114 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState116 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | CP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState118 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | FOR ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | IDENT _v ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | OA ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | RETURN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | STRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | INSTOF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | ME ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MULT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | NEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | OR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState123 in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState18 ->
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
            | MenhirState18 | MenhirState137 | MenhirState106 | MenhirState132 | MenhirState119 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
                let _v : (unit) =         (()) in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | MenhirState17 ->
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
            | MenhirState145 ->
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
            | MenhirState175 ->
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
    | MenhirState137 ->
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
    | MenhirState122 | MenhirState117 | MenhirState115 | MenhirState113 | MenhirState108 | MenhirState104 | MenhirState19 | MenhirState26 | MenhirState97 | MenhirState94 | MenhirState91 | MenhirState88 | MenhirState81 | MenhirState84 | MenhirState82 | MenhirState79 | MenhirState76 | MenhirState74 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState52 | MenhirState50 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, ie) = _menhir_stack in
        let _v : (unit) =                  (()) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 | MenhirState137 | MenhirState106 | MenhirState132 | MenhirState119 ->
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

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _2 = () in
    let _v : (unit) =                  (()) in
    _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
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
        | MenhirState21 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, ac) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                  (()) in
            _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
        | MenhirState36 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, ac) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                  (()) in
            _menhir_goto_instr_expr _menhir_env _menhir_stack _menhir_s _v
        | MenhirState104 | MenhirState113 | MenhirState115 | MenhirState117 | MenhirState122 | MenhirState108 | MenhirState19 | MenhirState26 | MenhirState97 | MenhirState94 | MenhirState91 | MenhirState30 | MenhirState88 | MenhirState50 | MenhirState79 | MenhirState81 | MenhirState84 | MenhirState82 | MenhirState56 | MenhirState58 | MenhirState76 | MenhirState74 | MenhirState60 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState62 | MenhirState64 | MenhirState52 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUN ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
            | OP ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
            | PUN ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
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
        | MenhirState18 | MenhirState137 | MenhirState106 | MenhirState132 | MenhirState119 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUN ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
            | OP ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
            | PUN ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
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
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let lit = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (unit) =                     (()) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let lit = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (unit) =                       (()) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState122 | MenhirState117 | MenhirState115 | MenhirState113 | MenhirState108 | MenhirState104 | MenhirState19 | MenhirState26 | MenhirState97 | MenhirState94 | MenhirState91 | MenhirState88 | MenhirState81 | MenhirState79 | MenhirState76 | MenhirState74 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState52 | MenhirState50 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState19 in
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | CONST_INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | CONST_STRING _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | IDENT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MUN ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | NEQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | NEW ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | NULL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | OP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PUN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MUN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
    | PT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
    | PUN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | CONST_INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | CONST_STRING _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | IDENT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MUN ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NEQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NEW ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NULL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | OP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PUN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | CONST_INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | CONST_STRING _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | IDENT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MUN ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEW ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NULL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | OP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PUN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
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
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | PUBLIC ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180

and _menhir_goto_loption_separated_nonempty_list_COMMA_param__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
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
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState143 ->
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
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | STRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | CA ->
        _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState21 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState26 in
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
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState26 in
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
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | MUN ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | PT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PUN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
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
        let _menhir_s = MenhirState26 in
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
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MINUS ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NEQ ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | OP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =         (()) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | CONST_STRING _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | IDENT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | MINUS ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | NEQ ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | NULL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | OP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | CP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState30 in
                let _v : (unit list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
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

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState36 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CONST_STRING _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | NULL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MUN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
    | PT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
    | PUN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_STRING _ | CP | EQUAL | IDENT _ | INSTOF | LE | LT | ME | MINUS | MT | MULT | NEQ | NEW | NULL | OP | OR | PLUS | SEMI ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    let _v : (unit) =                   (()) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int32) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (unit) =                ((*Int i*)()) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
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
    | MenhirState119 ->
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
    | MenhirState106 ->
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
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FOR ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | IDENT _v ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | MUN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NEW ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | OA ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | PUN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | RETURN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | STRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
        | BOOL | CA | FOR | IDENT _ | IF | INT | MUN | NEW | OA | PUN | RETURN | STRING | VOID ->
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
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, e), _), _, i1), _, i2) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (unit) =                                                                   (()) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState137 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | FOR ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | IDENT _v ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | IF ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | MUN ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | NEW ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | OA ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | PUN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | RETURN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | STRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | VOID ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | CA ->
            _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState143 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 ->
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

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FOR ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENT _v ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MUN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NEW ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | OA ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | PUN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | RETURN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | CA ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

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
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState156 ->
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
    | MenhirState143 | MenhirState14 | MenhirState8 ->
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
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                | STRING ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                | VOID ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
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
    | MenhirState137 | MenhirState18 | MenhirState132 | MenhirState106 | MenhirState119 ->
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
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | CONST_INT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | CONST_STRING _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | IDENT _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | MINUS ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | MUN ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | NEQ ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | NEW ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | NULL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | OP ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | PUN ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
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
    | MenhirState156 | MenhirState152 | MenhirState3 ->
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
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | STRING ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | VOID ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | CP ->
                    _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
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
                                                                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                                                | _ ->
                                                                    assert (not _menhir_env._menhir_error);
                                                                    _menhir_env._menhir_error <- true;
                                                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175)
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
    | MenhirState180 ->
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
    let _v : (unit) =           (()) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =        ((*TypInteger*)()) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | STRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | VOID ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | CP ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =         ((*TypBoolean*)()) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
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
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                    | IDENT _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                    | INT ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                    | STRING ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                    | VOID ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                    | CA ->
                        _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
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
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | STRING ->
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
  

