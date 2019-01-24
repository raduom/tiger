%{
    open Syntax
%}

(* Identifier *)
%token <string> ID

(* Strings  *)
%token <string> STRING

(* Numbers  *)
%token <int> INT

(* Functions *)
%token FUNCTION

(* Types *)
%token TYPE
%token STRINGTYPE
%token INTTYPE
%token ARRAY OF

(* Operators *)
%token EQUALS NOTEQUALS GT LT GTE LTE PLUS MINUS TIMES DIVIDE AND OR
%token COLON SEMICOLON
%token LPAREN RPAREN
%token LBRACE RBRACE

(* Records *)
%token LBRACKET RBRACKET
%token NIL UNIT
%token COMMA
%token DOT
%token VAR
%token ASSOC

(* Statements *)
%token LET IN END
%token IF THEN ELSE
%token FOR TO WHILE DO BREAK

(* The End *)
%token EOF

(* Precedence and associativity rules *)
%left OR
%left AND
%nonassoc GT GTE LT LTE EQUALS NOTEQUALS
%left PLUS MINUS
%left TIMES DIVIDE

%start <Syntax.tm option> prog

%%

prog:
  | EOF              { None }
  | t = expr; EOF    { Some t }
;

expr:
  (* Function application *)
  | f = ID; LPAREN; args = separated_list(COMMA, expr); RPAREN
    { App (f, args) }
  | l = expr; SEMICOLON; r = expr
    { Seq (l, r) }
  | l = expr; o = op; r = expr      { Op (o, l, r) }
  | IF; c = expr; THEN; t = expr; ELSE; e = expr
    { IfThenElse (c, t, e) }
  | LET; ds = list(decl); IN; body = option(expr); END
    { Let (ds, body) }
  | lv = lvalue; ASSOC; v = expr
    { Assoc (lv, v) }
  | IF; c = expr; THEN; t = expr
    { IfThen (c, t) }
  | WHILE; c = expr; DO; body = expr
    { While (c, body) }
  | FOR; id = ID; ASSOC; v0 = expr; TO; v1 = expr; DO; body = expr
    { For (id, v0, v1, body) }
  | t = ID; LBRACKET; size = expr; RBRACKET; OF; v = expr
    { NewArray (t, size, v) }
  | t = ID; LBRACE; vs = separated_list(COMMA, separated_pair(ID, COLON, expr)); RBRACE;
    { NewRecord (t, vs) }
  | NIL   { Nil   }
  | UNIT  { Unit  }
  | BREAK { Break }
  | v = STRING; { StringLiteral v }
  | v = INT; { IntLiteral v}
  | LPAREN; e = expr; RPAREN { e }
;

lvalue:
  | id = ID; LBRACKET; ix = INT; RBRACKET
    { ArrayIndex (id, ix) }
  | id = ID; DOT; p = ID;
    { RecordIndex (id, p) }
  | id = ID;
    { Id id }
;

decl:
  | TYPE; id = ID; EQUALS; t = ty
    { Type (id, t) }
  | VAR;  id = ID; t = option(ID); ASSOC; v = expr
    { Var (id, t, v) }
  | FUNCTION; id = ID; LPAREN; args = separated_list(COMMA, separated_pair(ID, COLON, ID)); RPAREN; t = option(preceded(COLON, ID)); EQUALS; body = expr
    { Function (id, args, t, body) }
;

ty:
  | STRINGTYPE { StringType }
  | INTTYPE    { IntType }
  | id = ID
    { TypeId id }
  | LBRACE; es = separated_list(COMMA, separated_pair(ID, COLON, ID)); RBRACE
    { Record es }
  | ARRAY; OF; id = ID
    { Array id }

%inline op:
  | NOTEQUALS { NotEquals }
  | EQUALS    { Equals    }
  | PLUS      { Plus      }
  | MINUS     { Minus     }
  | TIMES     { Times     }
  | DIVIDE    { Divide    }
  | GT        { Gt        }
  | LT        { Lt        }
  | GTE       { Gte       }
  | LTE       { Lte       }
  | AND       { And       }
  | OR        { Or        }
;
