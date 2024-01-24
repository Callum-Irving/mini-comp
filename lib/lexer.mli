(** The lexer type. *)
type t

(** Returns a new lexer on a string. *)
val make_lexer : string -> t

(** Gets the next token from a lexer, returning the new lexer and the token
    that was consumed. Returns EOF when it reaches the end of input. *)
val next_token : t -> t * Token.t

(** Consumes all tokens in lexer and returns them as a list. *)
val all_tokens : t -> Token.t list

(** Lexer received an unexpected character *)
exception UnexpectedChar of string * int
