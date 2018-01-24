
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
  | CONST_INT of (int)
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
  | MenhirState68
  | MenhirState63
  | MenhirState49
  | MenhirState44
  | MenhirState38
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState8
  | MenhirState3
  | MenhirState0
  


let rec _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_goto_class_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | PUBLIC ->
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, l), _, e) = _menhir_stack in
            let _2 = () in
            let _v : (unit) =                                 ((*Set(l,e)*)()) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Add*) ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Or*)  ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Neq*) ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Mult*)())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Mt *) ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Sub*) ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Me *) ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Lt *) ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Le *) ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*Eq*)  ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (unit) = let op =
              let _1 = _10 in
                           ( (*And*) ())
            in
                                                       ( (*Binop(op, e1, e2)*)() ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (unit) =               ((*Int i*)()) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let lit = _v in
    let _v : (unit) =                ( (*Literal(lit)*)()) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState63 | MenhirState44 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let is = _v in
        let _v : (unit) =                   (is) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState3 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), id), _, _4) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (unit) =                                 ((*id*)()) in
                _menhir_goto_class_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState44 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), id), id2), _, _6) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (unit) =                                                     ((*(id;id2)*)()) in
                _menhir_goto_class_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState63 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), id), main), str), arg), _, _15) = _menhir_stack in
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
                (match try
                  Some (                                                                                 ((*id*)(if main<>"main" || str <>"String" then (raise _eRR));()) : (unit))
                with
                | Error ->
                    None with
                | Some _v ->
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EOF ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, cds), _, mcd) = _menhir_stack in
                        let _3 = () in
                        let _v : ( unit ) =                                               ( (*cds@mcd*)() ) in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _1 = _v in
                        Obj.magic _1
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | None ->
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let is = _v in
        let (_menhir_stack, _menhir_s, i) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                                         ((* i :: is*)() ) in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce26 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _v : (unit) =            ((*identifier(id)*)()) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 | MenhirState44 | MenhirState38 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | IDENT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 | MenhirState32 | MenhirState30 | MenhirState28 | MenhirState26 | MenhirState24 | MenhirState22 | MenhirState20 | MenhirState18 | MenhirState16 | MenhirState14 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, loc) = _menhir_stack in
        let _v : (unit) =                 ((*Literal(lit)*)()) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | CA ->
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_class_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
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
            let _menhir_s = MenhirState49 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
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
                                                                | IDENT _v ->
                                                                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
                                                                | CA ->
                                                                    _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                                                                | _ ->
                                                                    assert (not _menhir_env._menhir_error);
                                                                    _menhir_env._menhir_error <- true;
                                                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
                                                            | _ ->
                                                                assert (not _menhir_env._menhir_error);
                                                                _menhir_env._menhir_error <- true;
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let (((((_menhir_stack, _menhir_s), _), _), _), _) = _menhir_stack in
                                                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                                        | _ ->
                                                            assert (not _menhir_env._menhir_error);
                                                            _menhir_env._menhir_error <- true;
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let (((((_menhir_stack, _menhir_s), _), _), _), _) = _menhir_stack in
                                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                                    | _ ->
                                                        assert (not _menhir_env._menhir_error);
                                                        _menhir_env._menhir_error <- true;
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let (((((_menhir_stack, _menhir_s), _), _), _), _) = _menhir_stack in
                                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_class_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =                                         ( (*[]*)()      ) in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MUN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                  (()) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | PUN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                  (()) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | SET ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
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

and _menhir_reduce23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_class_decl_ _menhir_env _menhir_stack _menhir_s _v

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
                    | IDENT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
                    | CA ->
                        _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
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
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | CA ->
                _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState3
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
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

