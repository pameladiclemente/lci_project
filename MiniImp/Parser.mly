%{
  open MiniImp
  open Printf
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token PLUS MINUS TIMES DIVIDE MODULO
%token LT LE GT GE EQ 
%token AND OR NOT
%token LPAREN RPAREN
%token DEF MAIN WITH INPUT OUTPUT AS
%token SKIP IF THEN ELSE WHILE DO 
%token ASSIGN SEQUENCE
%token EOF

%type <program> program
%type <cmd> cmd
%type <b_exp> b_exp
%type <a_exp> a_exp

%start program 

%left SEQUENCE
%right NOT
%left AND        
%left OR     
%left TIMES DIVIDE MODULO      
%left PLUS MINUS 

%%

program:
  | DEF; MAIN; WITH; INPUT; x = IDENT; OUTPUT; y = IDENT; AS; z = cmd; EOF   { Program (x, y, z) } 

cmd:
  | SKIP                                                    { Skip }
  | x = IDENT; ASSIGN; eval_a = a_exp;                      { Assign(x, eval_a) }
  | c1 = cmd; SEQUENCE; c2 = cmd;                           { Seq(c1, c2) }
  | IF; b = b_exp; THEN; b_true = cmd; ELSE; b_false = cmd; { If(b, b_true, b_false) }
  | WHILE; b = b_exp; DO; c1 = cmd;                         { While(b, c1) }


b_exp:
  | b = BOOL                        { Boolean(b) } 
  | b1 = b_exp; AND; b2 = b_exp     { And(b1, b2) } 
  | b1 = b_exp; OR; b2 = b_exp      { Or(b1, b2) } 
  | NOT; b = b_exp;                 { Not(b) } 
  | i1 = a_exp; LT; i2 = a_exp      { LessThan(i1, i2) } 
  | i1 = a_exp; LE; i2 = a_exp      { LessThanEqual(i1, i2) } 
  | i1 = a_exp; GT; i2 = a_exp      { GreaterThan(i1, i2) } 
  | i1 = a_exp; GE; i2 = a_exp      { GreaterThanEqual(i1, i2) } 
  | i1 = a_exp; EQ; i2 = a_exp      { Equal(i1, i2) } 
  | LPAREN; exp = b_exp; RPAREN       { exp }

a_exp:
  | i = INT                           { Integer(i) } 
  | x = IDENT                         { Variable(x) }
  | i1 = a_exp; PLUS; i2 = a_exp      { Add(i1, i2) } 
  | i1 = a_exp; MINUS; i2 = a_exp     { Sub(i1, i2) } 
  | i1 = a_exp; TIMES; i2 = a_exp     { Mul(i1, i2) } 
  | i1 = a_exp; DIVIDE; i2 = a_exp    { Div(i1, i2) } 
  | i1 = a_exp; MODULO; i2 = a_exp    { Mod(i1, i2) } 
  | LPAREN; exp = a_exp; RPAREN       { exp }


