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

%right SEQUENCE  
%right NOT       
%left AND        
%left OR         
%left PLUS MINUS 
%left TIMES DIVIDE MODULO  

%%

program:
  | DEF; MAIN; WITH; INPUT; x = IDENT; OUTPUT; y = IDENT; AS; z = cmd; EOF  
   { Program (x, y, z) } //def main with input x output y as c

cmd:
  | SKIP 
    { Skip } //skip 
  | a = IDENT; ASSIGN; eval_a = a_exp 
    { Assign(a, eval_a) } //x := a
  | eval_c1 = cmd; SEQUENCE; eval_c2 = cmd 
    { Seq(eval_c1, eval_c2) } //c1; c2
  | IF; b = b_exp; THEN; LPAREN; b_true = cmd; RPAREN; ELSE; LPAREN; b_false = cmd; RPAREN
    { If(b, b_true, b_false) } // if b then b_true else b_false
  | WHILE; b = b_exp; DO; LPAREN; c1 = cmd; RPAREN
    { While(b, c1) } // while b do c1

b_exp:
  | b = BOOL 
    { Boolean(b) } 
  | NOT; b = b_exp 
    { Not(b) } //not b
  | b1 = b_exp; AND; b2 = b_exp 
    { And(b1, b2) } //b1 && b2
  | b1 = b_exp; OR; b2 = b_exp 
    { Or(b1, b2) } //b1 || b2
  | i1 = a_exp; LT; i2 = a_exp 
    { LessThan(i1, i2) } //b1 < b2
  | i1 = a_exp; LE; i2 = a_exp 
    { LessThanEqual(i1, i2) } //b1 <= b2
  | i1 = a_exp; GT; i2 = a_exp 
    { GreaterThan(i1, i2) } //b1 > b2
  | i1 = a_exp; GE; i2 = a_exp 
    { GreaterThanEqual(i1, i2) } //b1 >= b2
  | i1 = a_exp; EQ; i2 = a_exp 
    { Equal(i1, i2) } //b1 == b2

a_exp:
  | i = INT 
    { Integer(i) } 
  | x = IDENT 
    { Variable(x) }
  | i1 = a_exp; PLUS; i2 = a_exp 
    { Add(i1, i2) } //i1 + i2
  | i1 = a_exp; MINUS; i2 = a_exp 
    { Sub(i1, i2) } //i1 - i2
  | i1 = a_exp; TIMES; i2 = a_exp 
    { Mul(i1, i2) } //i1 * i2
  | i1 = a_exp; DIVIDE; i2 = a_exp
    { Div(i1, i2) } //i1 / i2
  | i1 = a_exp; MODULO; i2 = a_exp 
    { Mod(i1, i2) } //i1 % i2
  | LPAREN; exp = a_exp; RPAREN 
    { (exp) } //(exp)


