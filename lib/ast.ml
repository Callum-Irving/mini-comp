(** An identifier. *)
type ident = string [@@deriving show]

(** A binary operator. *)
type binop =
  | Plus
  | Minus
  | Times
  | Divide
[@@deriving show]

(** An expression. *)
type expr =
  | Binary of expr * binop * expr
  | Let of ident * expr
  | Ident of ident
  | Int of int
[@@deriving show]

(** A program is a list of expressions. *)
type program = expr list [@@deriving show]
