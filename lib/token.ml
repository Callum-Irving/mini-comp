type token_type =
  | Ident of string
  | Literal of int
  | Plus
  | Minus
  | Times
  | Divide
  | Equals
  | LeftParen
  | RightParen
  | Newline
  | EOF
[@@deriving show]

type t =
  { kind : token_type
  ; posn : int
  ; line : int
  }
[@@deriving show]

let kind token = token.kind
