exception NotImplemented

let rec eval_expr expr =
  match expr with
  | Ast.Binary (lhs, op, rhs) ->
    let lval = eval_expr lhs in
    let rval = eval_expr rhs in
    (match op with
     | Ast.Plus -> lval + rval
     | Ast.Minus -> lval - rval
     | Ast.Times -> lval * rval
     | Ast.Divide -> lval / rval)
  | Ast.Let (name, expr) -> raise NotImplemented
  | Ast.Ident name -> raise NotImplemented
  | Ast.Int value -> value
;;
