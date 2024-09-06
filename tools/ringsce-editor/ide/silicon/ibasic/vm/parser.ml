{
open lexer
}

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "for" { FOR }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | double { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }