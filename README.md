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
