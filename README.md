# lci_project
LCI (Languages, Compilers and Interpreters), year 2024/2025
MiniImp Test and Compilation

First, generate the lexer and parser, and compile the core modules:

ocamllex Lexer.mll
ocamlc -c MiniImp.mli MiniImp.ml
menhir --infer Parser.mly
ocamlc -c Parser.mli Parser.ml Lexer.ml Interpreter.ml CFG.mli CFG.ml MiniRISC.mli MiniRISC.ml LivenessAnalysis.mli LivenessAnalysis.ml Compiler.ml MainCompiler.ml MainInterpreter.ml


Running the MiniImp Interpreter

To compile and run the MiniImp Interpreter:

ocamlc -o miniimp_int MiniImp.cmo Lexer.cmo Parser.cmo Interpreter.cmo MainInterpreter.ml
./miniimp_int test/sum.miniimp


This compiles the interpreter into an executable miniimp_int and runs it on the example file. It works via standard input and standard output: so it will be asked to the user to insert an input, and the output will be displayed subsequently (if computation ends without errors).

Running the MiniImp Compiler

To compile and run the MiniImp Compiler:

ocamlc -o miniimp_comp MiniImp.cmo CFG.cmo LivenessAnalysis.cmo MiniRISC.cmo Parser.cmo Lexer.cmo Compiler.cmo MainCompiler.ml
./miniimp_comp 8 test/sum.miniimp test/output.asm true false


This compiles the compiler logic into miniimp_comp. The execution command specifies 8 target registers, the input file, the output assembly path, enables the undefined variable check (true), and disables optimization (false). Differently from the Interpreter, the output will be written in a file at the given destination.

Clean Up

Clean up compiled files with:

rm *.cmo *.cmi Parser.ml Lexer.ml Parser.mli miniimp_int miniimp_comp


MiniFun Test

Similarly, for the functional language interpreter, generate the lexer and parser and compile the core modules:

ocamllex Lexer.mll
ocamlc -c MiniFun.mli MiniFun.ml
menhir --infer Parser.mly
ocamlc -c Parser.mli Parser.ml Lexer.ml Interpreter.ml


Running the MiniFun Interpreter

To compile and run the MiniFun Interpreter:

ocamlc -o minifun_int MiniFun.cmo Parser.cmo Lexer.cmo Interpreter.cmo
./minifun_int test/es1.minifun


Clean Up

Clean up compiled files with:

rm *.cmo *.cmi Parser.ml Lexer.ml Parser.mli minifun_int
