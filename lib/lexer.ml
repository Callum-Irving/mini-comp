(** Documented in lexer.mli *)
type t =
  { input : string
  ; ch : char option
  ; posn : int
  ; line : int
  }

(** Documented in lexer.mli *)
exception UnexpectedChar of string * int

exception LexerError of string

(** Utility function for determining if str is empty. *)
let is_empty_string str = String.length str = 0

(** Documented in lexer.mli *)
let make_lexer input =
  if is_empty_string input
  then { input; ch = None; posn = 0; line = 0 }
  else { input; ch = Some (String.get input 0); posn = 0; line = 0 }
;;

(** Advance lexer by one character. *)
let advance lexer =
  if lexer.posn >= String.length lexer.input - 1
  then { lexer with ch = None }
  else (
    let posn = lexer.posn + 1 in
    let ch = Some (String.get lexer.input posn) in
    match lexer.ch with
    | Some '\n' -> { lexer with ch; posn; line = lexer.line + 1 }
    | Some _ -> { lexer with ch; posn }
    | None -> raise (LexerError "lexer expected more input"))
;;

(** Advance lexer posn and ch until pred is met. *)
let skip_while lexer pred =
  let rec loop lexer = if pred lexer.ch then loop (advance lexer) else lexer in
  loop lexer
;;

(** Skip unimportant whitespace (everything except '\n'). *)
let skip_whitespace lexer =
  let is_whitespace ch =
    match ch with
    | Some (' ' | '\r' | '\t') -> true
    | _ -> false
  in
  skip_while lexer is_whitespace
;;

let string_of_char ch = String.make 1 ch

let take_literal (lexer : t) : t * Token.t =
  let posn = lexer.posn in
  let line = lexer.line in
  let rec take_chars lexer acc =
    match lexer.ch with
    | Some ('0' .. '9' as ch) -> take_chars (advance lexer) (acc ^ string_of_char ch)
    | Some (('a' .. 'z' | 'A' .. 'Z') as ch) ->
      raise
        (UnexpectedChar
           ("Unexpected character in integer literal: " ^ String.make 1 ch, lexer.posn))
    | _ -> lexer, Token.Literal (int_of_string acc)
  in
  let lexer, kind = take_chars lexer "" in
  lexer, { kind; posn; line }
;;

let take_ident lexer : t * Token.t =
  let posn = lexer.posn in
  let line = lexer.line in
  let rec take_chars lexer acc =
    match lexer.ch with
    | Some ch ->
      (match ch with
       | 'a' .. 'z' | 'A' .. 'Z' -> take_chars (advance lexer) (acc ^ string_of_char ch)
       | _ -> lexer, Token.Ident acc)
    | _ -> lexer, Token.Ident acc
  in
  let lexer, kind = take_chars lexer "" in
  lexer, { kind; posn; line }
;;

let token_at_posn lexer kind : Token.t = { kind; posn = lexer.posn; line = lexer.line }

(** Documented in lexer.mli *)
let next_token lexer : t * Token.t =
  let lexer = skip_whitespace lexer in
  let open Token in
  match lexer.ch with
  | None -> lexer, token_at_posn lexer EOF
  | Some ch ->
    (match ch with
     (* TODO: shrink code size. *)
     | '+' -> advance lexer, token_at_posn lexer Plus
     | '-' -> advance lexer, token_at_posn lexer Minus
     | '*' -> advance lexer, token_at_posn lexer Times
     | '/' -> advance lexer, token_at_posn lexer Divide
     | '=' -> advance lexer, token_at_posn lexer Equals
     | '(' -> advance lexer, token_at_posn lexer LeftParen
     | ')' -> advance lexer, token_at_posn lexer RightParen
     | '\n' -> advance lexer, token_at_posn lexer Newline
     | '0' .. '9' -> take_literal lexer
     | 'a' .. 'z' | 'A' .. 'Z' -> take_ident lexer
     | _ ->
       raise (UnexpectedChar ("Unexpected character: " ^ String.make 1 ch, lexer.posn)))
;;

(** Documented in lexer.mli *)
let all_tokens (lexer : t) : Token.t list =
  let rec aux lexer (acc : Token.t list) =
    match next_token lexer with
    | _, ({ kind = EOF; _ } as tok) -> tok :: acc
    | lexer, tok -> aux lexer (tok :: acc)
  in
  List.rev (aux lexer [])
;;
