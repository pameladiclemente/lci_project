/* Project fragment:
- Extend the concrete syntax with parenthesis for forcing the evaluation order 
(no need to change the abstract syntax)
- Define lexers and parsers for MiniImp and MiniFun (or MiniTyFun, as you prefer) 
by using ocamllex and menhir
- Get rid of ambiguities: menhir should not produce warnings!
*/

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
%token LPAREN RPAREN
%token IF THEN ELSE
%token LET LETFUN FUN IN ARROW
%token ASSIGN 
%token EOF 

%type <term> functionapplication
%type <term> atom
%type <term> term
%type <term> program

%start program

%left OR
%left AND
%left LT LE GT GE EQ
%left PLUS TIMES DIVIDE MODULO
%right NOT MINUS

%%

/* Added parentheses for evaluation order */

program:
  | t = term; EOF { t }

functionapplication: 
  | f = atom                    { f }
  | f = functionapplication; a = atom   { FunctionApplication(f, a) }

atom:
  | i = INT      { Integer(i) }
  | b = BOOL     { Boolean(b) }
  | x = IDENT    { Variable(x) }
  | LPAREN; t = term; RPAREN { t }
  | FUN; x = IDENT; ARROW; LPAREN; t = term; RPAREN { Function(x, t) }
  | IF; b = term; THEN; LPAREN; t1 = term; RPAREN; ELSE; LPAREN; t2 = term; RPAREN { If(b, t1, t2) }
  | LET; x = IDENT; ASSIGN; t1 = term; IN; LPAREN; t2 = term; RPAREN; { Let(x, t1, t2) }
  | LETFUN; f = IDENT; x = IDENT; ASSIGN; t1 = term; IN; LPAREN; t2 = term; RPAREN; { LetFun(f, x, t1, t2) }

term:
  | f = functionapplication                                         { f }
  | i1 = term; PLUS; i2 = term                                      { Add(i1, i2) } 
  | i1 = term; MINUS; i2 = term                                     { Sub(i1, i2) } 
  | i1 = term; TIMES; i2 = term                                     { Mul(i1, i2) } 
  | i1 = term; DIVIDE; i2 = term                                    { Div(i1, i2) } 
  | i1 = term; MODULO; i2 = term                                    { Mod(i1, i2) }
  | b1 = term; AND; b2 = term                                       { And(b1, b2) } 
  | b1 = term; OR; b2 = term                                        { Or(b1, b2) } 
  | NOT;  x = term;                                                 { Not(x) }
  | MINUS; x = term;                                                { NotInt(x) } 
  | i1 = term; LT; i2 = term                                        { LessThan(i1, i2) } 
  | i1 = term; LE; i2 = term                                        { LessThanEqual(i1, i2) } 
  | i1 = term; GT; i2 = term                                        { GreaterThan(i1, i2) } 
  | i1 = term; GE; i2 = term                                        { GreaterThanEqual(i1, i2) } 
  | i1 = term; EQ; i2 = term                                        { Equal(i1, i2) }  

