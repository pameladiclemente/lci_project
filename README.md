# LCI (Languages, Compilers and Interpreters), year 2024/2025

# MiniImp Test
First, enter the appropriate folder:
```bash
cd MiniImp
```

If in ```MiniFun``` folder, perform:
```bash
cd ../MiniImp
```

Then, generate generate the lexer and parser, and compile the core modules:
```bash
ocamllex Lexer.mll
ocamlc -c MiniImp.mli MiniImp.ml
menhir --infer Parser.mly
ocamlc -c Parser.mli Parser.ml Lexer.ml Interpreter.ml CFG.mli CFG.ml MiniRISC.mli MiniRISC.ml LivenessAnalysis.mli LivenessAnalysis.ml Compiler.ml MainCompiler.ml MainInterpreter.ml
```

### • Running the MiniImp Interpreter
To compile and run the MiniImp Interpreter:
```bash
ocamlc -o miniimp_int MiniImp.cmo Lexer.cmo Parser.cmo Interpreter.cmo MainInterpreter.ml
```
This compiles the interpreter into an executable ```miniimp_int```.

```bash
./miniimp_int test/es1.miniimp
```
The executable is run on the example file. It works via standard input and standard output: so it will be asked to the user to insert an input, and the output will be displayed subsequently (if computation ends without errors). 

### • Running the MiniImp Compiler
To compile and run the MiniImp Compiler:

ocamlc -o miniimp_comp MiniImp.cmo CFG.cmo LivenessAnalysis.cmo MiniRISC.cmo Parser.cmo Lexer.cmo Compiler.cmo MainCompiler.ml

This compiles the compiler logic into ```miniimp_comp```. 

```bash
./miniimp_comp 4 test/es1.miniimp test/output-es1.asm false false
```
The execution command specifies 4 target registers, the input file, the output assembly path, disables both the undefined variable check and optimization (\texttt{false}). Differently from the Interpreter, the output will be written in a file at the given destination \texttt{test/output-es1.asm}.

## Clean Up
Clean up compiled files with:

```bash
rm *.cmo *.cmi Parser.ml Lexer.ml Parser.mli miniimp_int miniimp_comp test/*.asm
```
Example files go from \texttt{es1} to \texttt{es6}, where \texttt{es6} has been specifically written to check Defined Variables Analysis correct functioning. 

# MiniFun Test
Similarly, for the functional language interpreter, first enter the appropriate folder:
```bash
cd MiniFun
```

If in ```MiniImp``` folder, perform:
```bash
cd ../MiniFun
```

Then, generate the lexer and parser and compile the core modules:
```bash
ocamllex Lexer.mll
ocamlc -c MiniFun.mli MiniFun.ml
menhir --infer Parser.mly
ocamlc -c Parser.mli Parser.ml Lexer.ml Interpreter.ml
```

## Running the MiniFun Interpreter
To compile and run the MiniFun Interpreter:
```bash
ocamlc -o minifun_int MiniFun.cmo Parser.cmo Lexer.cmo Interpreter.cmo
```
```bash
./minifun_int test/es1.minifun
```
Examples go from \textt{ex1} to \texttt{ex5}.

## Clean Up
Clean up compiled files with:
```bash
rm *.cmo *.cmi Parser.ml Lexer.ml Parser.mli minifun_int
```

# MiniTyFun Test
Similarly, first enter the appropriate folder:
```bash
cd MiniFun
```

If in ```MiniImp``` folder, perform:
```bash
cd ../MiniFun
```

Then, compile the core modules:
```bash
ocamlc -c MiniTyFun.mli MiniTyFun.ml TypeChecker.ml
```

Execute the Type Checker:
```bash
ocamlc -o minityfun_test MiniTyFun.cmo TypeChecker.cmo

./minityfun_test
```

In the standard output, it will be visible the result of three test contained in \texttt{TypeChecker.ml}.

## Clean Up
Clean up compiled files with:
```bash
rm *.cmo *.cmi minityfun_test
```
