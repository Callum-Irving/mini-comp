(** The parser type. *)
type t

(** Create a parser from a list of tokens. *)
val make_parser : Lexer.t -> t

(** TODO: Add documentation *)
val parse_expr : t -> t * Ast.expr

(** TODO: Add documentation *)
val parse_expr_list : t -> t * Ast.expr list
