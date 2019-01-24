{
  open Lexing
  open TigerParser

  exception Error of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']
let int = '-'? ['0'-'9']+

rule read =
  parse

  (* White space *)
  | newline                   { new_line lexbuf; read lexbuf }
  | white                     { read lexbuf }

  (* Operators *)
  | "="                       { EQUALS }
  | "<>"                      { NOTEQUALS }
  | ">"                       { GT }
  | "<"                       { LT }
  | ">="                      { GTE }
  | "<="                      { LTE }
  | "+"                       { PLUS }
  | "-"                       { MINUS }
  | "*"                       { TIMES }
  | "/"                       { DIVIDE }
  | "&"                       { AND }
  | "|"                       { OR }

  (* Separators *)
  | ":"                       { COLON }
  | ";"                       { SEMICOLON }
  | ","                       { COMMA }

  (* Grouping *)
  | "("                       { LPAREN }
  | ")"                       { RPAREN }
  | "{"                       { LBRACE }
  | "}"                       { RBRACE }
  | "["                       { LBRACKET }
  | "]"                       { RBRACKET }

  (* Identifiers *)
  | "type"                    { TYPE       }
  | "function"                { FUNCTION   }
  | "let"                     { LET        }
  | "in"                      { IN         }
  | "end"                     { END        }
  | "array"                   { ARRAY      }
  | "of"                      { OF         }
  | "var"                     { VAR        }
  | ":="                      { ASSOC      }
  | "nil"                     { NIL        }
  | "()"                      { UNIT       }
  | "."                       { DOT        }
  | "if"                      { IF         }
  | "then"                    { THEN       }
  | "else"                    { ELSE       }
  | "for"                     { FOR        }
  | "to"                      { TO         }
  | "while"                   { WHILE      }
  | "do"                      { DO         }
  | "break"                   { BREAK      }
  | "string"                  { STRINGTYPE }
  | "int"                     { INTTYPE    }

  (* Primitives *)
  | '"'                       { read_string (Buffer.create 42) lexbuf }
  | "/*"                      { read_comment 1 lexbuf }
  | int as it                 { INT (int_of_string it) }
  | id  as it                 { ID it }

and read_comment depth =
  parse

  | "/*"                      { read_comment (depth + 1) lexbuf }
  | "*/"                      { if (depth == 1)
                                then read lexbuf
                                else read_comment (depth - 1) lexbuf }

and read_string buf =
  parse

  | '"'                       { STRING (Buffer.contents buf) }
  | '\\' '\\'                 { Buffer.add_char buf '\\'   ; read_string buf lexbuf }
  | '\\' '/'                  { Buffer.add_char buf '/'    ; read_string buf lexbuf }
  | '\\' 'b'                  { Buffer.add_char buf '\b'   ; read_string buf lexbuf }
  | '\\' 'f'                  { Buffer.add_char buf '\012' ; read_string buf lexbuf }
  | '\\' 'n'                  { Buffer.add_char buf '\n'   ; read_string buf lexbuf }
  | '\\' 'r'                  { Buffer.add_char buf '\r'   ; read_string buf lexbuf }
  | '\\' 't'                  { Buffer.add_char buf '\t'   ; read_string buf lexbuf }
  | [^ '"' '\\']+             { Buffer.add_string buf (lexeme lexbuf);
                                read_string buf lexbuf }
  | _                         { raise (Error ("Illegal string character: " ^ lexeme lexbuf)) }
  | eof                       { raise (Error ("String is not terminated")) }
