(** Documented in parser.mli *)
type t =
  { lookahead : Token.t
  ; lexer : Lexer.t
  }

exception UnexpectedToken of string
exception UnexpectedEOF
exception NotImplemented of string

(** Documented in parser.mli *)
let make_parser lexer =
  let lexer, lookahead = Lexer.next_token lexer in
  { lookahead; lexer }
;;

(** Advance the parser by one token. Returns the new parser and Some token or
    None depending on if there was another token or not. *)
let advance parser =
  match parser.lookahead.kind with
  | Token.EOF -> raise UnexpectedEOF
  | _ ->
    let lexer, lookahead = Lexer.next_token parser.lexer in
    { lookahead; lexer }
;;

(** Advance parser if parser.lookahead matches token, otherwise returns None. *)
let expect token parser =
  if parser.lookahead.kind = token then Some (advance parser) else None
;;

(** Advance parser while the current token matches pred. *)
let rec skip_while parser pred =
  if pred parser.lookahead then skip_while (advance parser) pred else parser
;;

(** Consume an identifier of integer expression from parser. *)
let rec parse_factor parser =
  match parser.lookahead.kind with
  | Token.LeftParen ->
    let parser = advance parser in
    let parser, inner_expr = parse_expr parser in
    parser
    (* TODO: Ugly *)
    |> expect Token.RightParen
    |> Option.map (fun parser -> parser, inner_expr)
    |> Option.get
  | Token.Ident name -> advance parser, Ast.Ident name
  | Token.Literal value -> advance parser, Ast.Int value
  | tok ->
    raise (NotImplemented ("expected literal or ident, got " ^ Token.show_token_type tok))

(** Consume a multiplication or division binary expression from parser. *)
and parse_term parser =
  let parser, first_factor = parse_factor parser in
  let try_factor parser =
    match parser.lookahead.kind with
    | Token.Times ->
      (* TODO: Ugly *)
      let parser, factor = advance parser |> parse_factor in
      parser, Some (Ast.Times, factor)
    | Token.Divide ->
      let parser, factor = advance parser |> parse_factor in
      parser, Some (Ast.Divide, factor)
    | _ -> parser, None
  in
  let rec loop_factors parser expr =
    match try_factor parser with
    | parser, Some (op, factor) -> loop_factors parser (Ast.Binary (expr, op, factor))
    | parser, None -> parser, expr
  in
  loop_factors parser first_factor

(** Consume an addition or subtraction binary expression from parser. *)
and parse_expr parser =
  let parser, first_term = parse_term parser in
  let try_term parser =
    match parser.lookahead.kind with
    | Token.Plus ->
      let parser, term = advance parser |> parse_term in
      parser, Some (Ast.Plus, term)
    | Token.Minus ->
      let parser, term = advance parser |> parse_term in
      parser, Some (Ast.Minus, term)
    | _ -> parser, None
  in
  let rec loop_terms parser expr =
    match try_term parser with
    | new_parser, Some (op, term) -> loop_terms new_parser (Ast.Binary (expr, op, term))
    | new_parser, None -> new_parser, expr
  in
  loop_terms parser first_term

(** Parse a list of expressions. *)
and parse_expr_list parser =
  let parser, first_expr = parse_expr parser in
  let try_expr parser =
    match parser.lookahead.kind with
    | Token.Newline ->
      let parser = skip_while parser (fun tok -> tok.kind = Token.Newline) in
      if parser.lookahead.kind = Token.EOF
      then parser, None
      else (
        let parser, expr = parse_expr parser in
        parser, Some expr)
    | Token.EOF -> parser, None
    | other ->
      raise
        (UnexpectedToken ("expected EOF or newline, found " ^ Token.show_token_type other))
  in
  let rec loop_exprs parser acc =
    match try_expr parser with
    | parser, Some expr -> loop_exprs parser (expr :: acc)
    | parser, None -> parser, acc
  in
  let parser, exprs = loop_exprs parser [ first_expr ] in
  parser, List.rev exprs
;;
