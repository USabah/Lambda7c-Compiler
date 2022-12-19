============================LAMBDA7C COMPILER============================

Lambda7c is a programming language built using fsharp by Uriya Sabah with 
the purpose of learning about each stage of compilation and the minute 
details in the process of building a programming language. It uses prefix 
syntax, as its abstract syntax tree is generated using an LL(1) grammer. 
While it is limited in that it doesn't contain libraries or frameworks, 
but since it compiles to LLVM (which can then be compiled using clang), 
it makes use of functions in C's library which allows it to function as a 
basic, yet intuitive programming language. 


================================HOW TO RUN===============================

You can run Lambda7c using the following commands:

First, download the directory from the zip on GitHub or run 
  git clone https://github.com/USabah/Lambda7c-Compiler.git

Then, (optionally) compile the source code by executing the compilation 
script:
  bash full_compile.exe

Next, you can create a script file (optionally labeled as a .7c file,
see the tests directory for some sample programs!) and run it using
  mono lambda7c_main.exe YOUR/FILE/PATH

Or you can input an expression interactively by running
  mono lambda7c_main.exe

You can also optionally pass the -generate and/or -trace flags 
to re-generate the parsing table, or trace the compilation as its
completed. 

The last stage will create an output file (output.ll) which can be
compiled with:
  clang output.ll cheats7c.o

and ran with:
  ./a.out

==============================ORGANIZATION===============================

Listed below are the relevant files to the compilation of lambda7c code:

lambda7c_main.fs: The runner of the system. Takes input from the user via
file or command line, and runs the parser, type checker, and compiler. 
It will then generate an output.ll file with the program compiled to LLVM. 

output.ll: Destination of the LLVM compiled program.

recless.fs: Defines the parser functions and classes. Parses the input 
using LL(1) Grammar production rules, which are defined in Lamecbda7c_ast.fs. 

lambda7c_ast.fs: Defines the LL(1) Grammar rules to produce the abstract
syntax tree of the language, which can then be used to type check and compile
the programs.

lamdba7c_typing.fs: The type checker of the compiler. Recursively infers the
type of each expression in the abstract syntax tree to determine whether the
program is type safe. Defines the symbol table which contains information
about each table frame and the table entries within them.

llvmir.fs: Defines the simplified abstract representation of LLVM IR. This
also includes the to_string method(s) which converts the representation to
LLVM code. Contains the definition

llvm_compiler.fs: Contains the definitions of the compiler, which converts the 
abstract syntax tree of lambda7c to the abstract syntax of LLVM, which is
converted to string format in llvmir.fs. 


================================FEATURES=================================

Lambda7c currently supports the following operations:

Note: types that are currently supported in definitions are 
  int float string

Binary operations: (op expr1 expr2)
  where op can be any of: 
    Mathematical Operators: + - * / % 
    Boolean Operators: ^ = < > <= >= neq eq and or

Uniary operations: (op expr1)
  where op can be any of: 
    Negation: ~ 
    Boolean Negation: not 
    Standard Output: display

If Else Statement: (if (cond) (expr1) (expr2))

While Loop: (while (cond) (expr1))

Variable Definition: (define var (expr1))

Typed Definition: (define var:type (expr1))

Destructive Assignment: (setq var (expr1))

Function Definition: (define var (lambda (arg1:type arg2:type ...) (expr1)))

Typed Function Definition: 
  (define var (lambda (arg1:type arg2:type ...):type (expr1)))

Let Expression: (let (var:type) (expr1) (expr2))

Typed Let Expression: (let (var) (expr1) (expr2))

Begin Sequences: (begin (expr1) (expr2) (expr3)......)

Sequences: ((expr1) (expr2) (expr3)......)

Closure Definitions must be exported globally using
  (export closure_name)

Expressions can be evaluated to Int, Float, String, Closures, or Arrays

=========================================================================
See the tests directory for some sample programs to compile!
