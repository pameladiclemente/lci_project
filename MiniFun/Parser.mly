%{
  open MiniFun
  open Printf
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token PLUS MINUS TIMES DIVIDE MODULO
%token LT LE GT GE EQ
%token AND OR NOT
%token IF THEN ELSE
%token LET LETFUN FUN IN ARROW ASSIGN 
%token EOF LPAREN RPAREN

%type <term> term
%type <term> program
%type <term> param

%start program

/* lowest precedence */
%nonassoc ELSE
%left IN
%left AND
%left OR
%left LT LE GT GE EQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left NOT
%%

program:
| app = term; EOF { app }

param:
| i = INT                                                         { Integer(i) } 
| b = BOOL                                                        { Boolean(b) } 
| x = IDENT                                                       { Variable(x) }

term:
| i = INT                                                          { Integer(i) } 
| b = BOOL                                                        { Boolean(b) } 
| x = IDENT                                                       { Variable(x) }
| FUN; t1 = IDENT; ARROW; t2 = param;                              { Function(t1, t2) }
| f = IDENT; t2 = param;                                           { FunctionApplication(f, t2) }
| i1 = term; PLUS; i2 = term                                      { Add(i1, i2) } 
| i1 = term; MINUS; i2 = term                                     { Sub(i1, i2) } 
| i1 = term; TIMES; i2 = term                                     { Mul(i1, i2) } 
| i1 = term; DIVIDE; i2 = term                                    { Div(i1, i2) } 
| i1 = term; MODULO; i2 = term                                    { Mod(i1, i2) }
| b1 = term; AND; b2 = term                                       { And(b1, b2) } 
| b1 = term; OR; b2 = term                                        { Or(b1, b2) } 
| NOT;  x = term;                                                 { Not(x) } 
| MINUS;  x = term;                                               { Not(x) } 
| i1 = term; LT; i2 = term                                        { LessThan(i1, i2) } 
| i1 = term; LE; i2 = term                                        { LessThanEqual(i1, i2) } 
| i1 = term; GT; i2 = term                                        { GreaterThan(i1, i2) } 
| i1 = term; GE; i2 = term                                        { GreaterThanEqual(i1, i2) } 
| i1 = term; EQ; i2 = term                                        { Equal(i1, i2) } 
| IF; b = term; THEN; b_true = term; ELSE; b_false = term;        { If(b, b_true, b_false) } 
| LET; x = IDENT; ASSIGN; t1 = term; IN; t2 = term;                { Let(x, t1, t2) }
| LETFUN; f = IDENT; t1 = IDENT; ASSIGN; t2 = term; IN; t3 = term;   { LetFun(f, t1, t2, t3) }
| LPAREN; t = term; RPAREN { t }
