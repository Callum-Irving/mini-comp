open Mini_comp

let filename = "test_input.mini"

let () =
  let ic = open_in filename in
  let file_str = In_channel.input_all ic in
  let lexer = Lexer.make_lexer file_str in
  let tokens = Lexer.all_tokens lexer in
  let parser = Parser.make_parser tokens in
  let _, exprs = Parser.parse_expr_list parser in
  let rec print_exprs_results exprs =
    match exprs with
    | hd :: tl ->
      print_endline (string_of_int (Evaluator.eval_expr hd));
      print_exprs_results tl
    | [] -> ()
  in
  print_exprs_results exprs
;;
