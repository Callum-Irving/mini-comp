# Mini Compiler

Example:

```
a = 1 + 2
3 * 4 + 2 * 4
4 * a
```

The result of each line is printed to stdout if not assigned to a variable.

## Tokens

- `IDENT`: a variable name
  - starts with `_ | a..z | A..Z`
- `LITERAL`: an integer
- `PLUS`: '+'
- `MINUS`: '-'
- `TIMES`: '*'
- `DIVIDE`: '/'
- `LPAREN`: '('
- `RPAREN`: ')'
- `NEWLINE`: '\n'
- `EQUALS`: '='

## To-do

- Optimize parser code (there are repeated patterns that could probably be
  extracted to functions).
- Fill in documentation.
- In the future, the lexer doesn't need to generate the entire list of tokens
  before parsing. Instead, the parser can parse as the lexer generates tokens.
