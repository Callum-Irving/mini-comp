(** Documented in parser.mli *)
type t =
  { tokens : Token.t list
  ; lookahead : Token.t option (* TODO: probably don't need *)
  }

exception UnexpectedToken of string
exception UnexpectedEOF
exception NotImplemented of string
(* exception UnexpectedToken of Token.t *)

(** Documented in parser.mli *)
let make_parser tokens =
  match tokens with
  | hd :: _ -> { tokens; lookahead = Some hd }
  | _ -> { tokens; lookahead = None }
;;

let advance parser =
  match parser.tokens with
  | _ :: (lookahead :: _ as tokens) -> { tokens; lookahead = Some lookahead }
  | _ :: [] -> { tokens = []; lookahead = None }
  | [] -> raise (NotImplemented "couldn't advance parser")
;;

let rec skip_while parser pred =
  if pred parser.lookahead then skip_while (advance parser) pred else parser
;;

(** Consume an identifier of integer expression from parser. *)
let rec parse_factor parser =
  match parser.lookahead with
  | Some Token.LeftParen ->
    let parser = advance parser in
    let parser, inner_expr = parse_expr parser in
    (match parser.lookahead with
     | Some Token.RightParen -> advance parser, inner_expr
     | _ -> raise (UnexpectedToken "unexpected token when parsing factor, expected ')'"))
  | Some (Token.Ident name) -> advance parser, Ast.Ident name
  | Some (Token.Literal value) -> advance parser, Ast.Int value
  | Some tok ->
    raise (NotImplemented ("expected literal or ident, got " ^ Token.show tok))
  | None -> raise UnexpectedEOF

(** Consume a multiplication or division binary expression from parser. *)
and parse_term parser =
  let parser, first_factor = parse_factor parser in
  let try_factor parser =
    match parser.lookahead with
    | Some Token.Times ->
      let parser, factor = advance parser |> parse_factor in
      parser, Some (Ast.Times, factor)
    | Some Token.Divide ->
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
    match parser.lookahead with
    | Some Token.Plus ->
      let parser, term = advance parser |> parse_term in
      parser, Some (Ast.Plus, term)
    | Some Token.Minus ->
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
    match parser.lookahead with
    | Some Token.Newline ->
      let parser = skip_while parser (( = ) (Some Token.Newline)) in
      if parser.lookahead = Some Token.EOF
      then parser, None
      else (
        let parser, expr = parse_expr parser in
        parser, Some expr)
    | Some Token.EOF -> parser, None
    | Some other -> raise (UnexpectedToken "expected EOF or newline, found other")
    | None -> raise UnexpectedEOF
  in
  let rec loop_exprs parser acc =
    match try_expr parser with
    | parser, Some expr -> loop_exprs parser (expr :: acc)
    | parser, None -> parser, acc
  in
  let parser, exprs = loop_exprs parser [ first_expr ] in
  parser, List.rev exprs
;;
