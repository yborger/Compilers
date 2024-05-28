%{
  open Batteries;;

  open Asts;;
%}

%token <string> INTEGER
%token <string> IDENTIFIER
%token AFTER BEFORE LET EQUAL IN LPAREN RPAREN PLUS MINUS TIMES EOF

%right IN
%left PLUS MINUS
%left TIMES

%type <Asts.expr> program

%start program

%%

program:
  | expr EOF { $1 }

expr:
  | constant { $1 }
  | AFTER LPAREN expr RPAREN { EUnaryOp(OpAfter,$3) }
  | BEFORE LPAREN expr RPAREN { EUnaryOp(OpBefore,$3) }
  | LPAREN expr RPAREN { $2 }
  | LET ident EQUAL expr IN expr { ELet($2,$4,$6) }
  | expr PLUS expr { EBinaryOp(OpPlus,$1,$3) }
  | expr MINUS expr { EBinaryOp(OpMinus,$1,$3) }
  | expr TIMES expr { EBinaryOp(OpTimes,$1,$3) }
  | IDENTIFIER { EVar($1) }

constant:
  | INTEGER { EInt(int_of_string $1) }
  | MINUS INTEGER { EInt(int_of_string ("-" ^ $2)) }

ident:
  | IDENTIFIER { $1 }

%%
