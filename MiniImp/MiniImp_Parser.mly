%{
  open Ast
  open Lexer
  open Printf
%}

(* %token: We declare the tokens recognized by the lexer. This matches the token names defined in your lexer (e.g., INT, IDENT, PLUS, MINUS, etc.).*)
%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token PLUS MINUS TIMES DIVIDE MODULO
%token LT LE GT GE EQ BOOL
%token AND OR NOT
%token ASSIGN SEQUENCE
%token LPAREN RPAREN
%token DEF MAIN WITH INPUT OUTPUT AS
%token SKIP IF THEN ELSE WHILE DO
%token EOF

%type <program> program
%type <cmd> cmd
%type <b_exp> b_exp
%type <a_exp> a_exp

%left AND OR 
%left EQ LT LE GT GE (* Boolean operators have lower precedence *)
%left PLUS MINUS 
%left TIMES DIVIDE MODULO  (* Arithmetic addition and subtraction *)

%start program (*%start: The entry point of the parser is the program rule, which returns an AST (Ast.program).*)

%%
(* Precedence rules (%left and %right):
AND and OR have lower precedence (boolean operators).
EQ, LT, LE, GT, and GE (comparison operators) have higher precedence than AND and OR.
PLUS, MINUS have higher precedence than AND and OR, but lower than TIMES, DIVIDE, and MODULO.
TIMES, DIVIDE, and MODULO have the highest precedence for arithmetic operators.
ASSIGN has the lowest precedence (since assignments are evaluated last). *)


(*The grammar:
program: Defines a program with the syntax def <input> <output> as <cmd>. The body of the program is a cmd (a command).
cmd: Defines different types of commands like skip, assignments, if-then-else, and while loops. The Seq rule allows sequencing commands with a semicolon.
a_exp: Defines arithmetic expressions, including integers, variables, and binary operations like addition, subtraction, multiplication, division, and modulo. Parentheses are used for grouping.
b_exp: Defines boolean expressions, including true, false, not, logical and, or, and comparison operators (<, <=, >, >=, ==). *)
program:
  | DEF; MAIN; WITH; INPUT; x = IDENT; OUTPUT; y = IDENT; AS; z = cmd; EOF 
  cmd 
    { Program (x, y, cmd) } //def main with input x output y as c

cmd:
  | SKIP 
    { Skip } //skip 
  | a = IDENT; ASSIGN; eval_a = a_exp 
    { Assign (a, eval_a) } //x := a
  | eval_c1 = cmd; SEQUENCE; eval_c2 = cmd 
    { Seq (eval_c1, eval_c2) } //c1; c2
  | IF; b = b_exp; THEN; b_true = cmd; ELSE; b_false = cmd 
    { If (b, b_true, b_false) } // if b then b_true else b_false
  | WHILE; b = b_exp; DO; c1 = cmd 
    { While (b, c1) } // while b do c1

b_exp:
  | b = BOOL 
    { Boolean (b) } 
  | NOT; b = b_exp 
    { Not (b) } //not b
  | b1 = b_exp; AND; b2 = b_exp 
    { And (b1, b2) } //b1 && b2
  | b1 = b_exp; OR; b2 = b_exp 
    { Or (b1, b2) } //b1 || b2
  | i1 = a_exp; LT; i2 = a_exp 
    { LessThan (i1, i2) } //b1 < b2
  | i1 = a_exp; LE; i2 = a_exp 
    { LessThanEqual (i1, i2) } //b1 <= b2
  | i1 = a_exp; GT; i2 = a_exp 
    { GreaterThan (i1, i2) } //b1 > b2
  | i1 = a_exp; GE; i2 = a_exp 
    { GreaterThanEqual (i1, i2) } //b1 >= b2
  | i1 = a_exp; EQ; i2 = a_exp 
    { Equal (i1, i2) } //b1 == b2

a_exp:
  | i = INT 
    { Integer (i) } 
  | x = IDENT 
    { Variable (x) }
  | i1 = a_exp; PLUS; i2 = a_exp 
    { Add (i1, i2) } //i1 + i2
  | i1 = a_exp; MINUS; i2 = a_exp 
    { Sub (i1, i2) } //i1 - i2
  | i1 = a_exp; TIMES; i2 = a_exp 
    { Mul (i1, i2) } //i1 * i2
  | i1 = a_exp; DIVIDE; i2 = a_exp
    { Div (i1, i2) } //i1 / i2
  | i1 = a_exp; MODULO; i2 = a_exp 
    { Mod (i1, i2) } //i1 % i2
  | LPAREN; exp = a_exp; RPAREN 
    { (exp) } //(exp)


