(** Documented in lexer.mli *)
type t =
  { input : string
  ; ch : char option
  ; posn : int
  }

(** Documented in lexer.mli *)
exception UnexpectedChar of string * int

(** Utility function for determining if str is empty. *)
let is_empty_string str = String.length str = 0

(** Documented in lexer.mli *)
let make_lexer input =
  if is_empty_string input
  then { input; ch = None; posn = 0 }
  else { input; ch = Some (String.get input 0); posn = 0 }
;;

(** Advance lexer by one character. *)
let advance lexer =
  if lexer.posn >= String.length lexer.input - 1
  then { lexer with ch = None }
  else (
    let posn = lexer.posn + 1 in
    { lexer with ch = Some (String.get lexer.input posn); posn })
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

let take_literal lexer =
  let rec take_chars lexer acc =
    match lexer.ch with
    | Some ('0' .. '9' as ch) -> take_chars (advance lexer) (acc ^ string_of_char ch)
    | Some (('a' .. 'z' | 'A' .. 'Z') as ch) ->
      raise
        (UnexpectedChar
           ("Unexpected character in integer literal: " ^ String.make 1 ch, lexer.posn))
    | _ -> lexer, Token.Literal (int_of_string acc)
  in
  take_chars lexer ""
;;

let take_ident lexer =
  let rec take_chars lexer acc =
    match lexer.ch with
    | Some ch ->
      (match ch with
       | 'a' .. 'z' | 'A' .. 'Z' -> take_chars (advance lexer) (acc ^ string_of_char ch)
       | _ -> lexer, Token.Ident acc)
    | _ -> lexer, Token.Ident acc
  in
  take_chars lexer ""
;;

(** Documented in lexer.mli *)
let next_token lexer =
  let lexer = skip_whitespace lexer in
  let open Token in
  match lexer.ch with
  | None -> lexer, EOF
  | Some ch ->
    (match ch with
     | '+' -> advance lexer, Plus
     | '-' -> advance lexer, Minus
     | '*' -> advance lexer, Times
     | '/' -> advance lexer, Divide
     | '=' -> advance lexer, Equals
     | '(' -> advance lexer, LeftParen
     | ')' -> advance lexer, RightParen
     | '\n' -> advance lexer, Newline
     | '0' .. '9' -> take_literal lexer
     | 'a' .. 'z' | 'A' .. 'Z' -> take_ident lexer
     | _ ->
       raise (UnexpectedChar ("Unexpected character: " ^ String.make 1 ch, lexer.posn)))
;;

(** Documented in lexer.mli *)
let all_tokens lexer =
  let rec aux lexer acc =
    match next_token lexer with
    | _, EOF -> Token.EOF :: acc
    | lexer, tok -> aux lexer (tok :: acc)
  in
  List.rev (aux lexer [])
;;
