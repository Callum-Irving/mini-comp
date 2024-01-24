type t =
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
