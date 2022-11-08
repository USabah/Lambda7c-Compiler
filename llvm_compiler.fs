module lambda7c
open System
open System.Collections.Generic
open lambda7c
open Recless.Base
open Option

//fsharpc llvm_compiler.fs -r lambda7c_typing.dll -r llvmir.dll

//Compiler
type LLVMCompiler =
{
  symbol_table : SymbolTable,
  program: LLVMProgram,  // to be built
  gindex: int, // global counter
  lindex: int, // local counter, set to 0 prior to compiling functions
  // .. other stuff omitted
} //LLVMCompiler

  //returns the destination register of the expression (of type LLVMexpr)
  member this.compile_expr(expression:LBox<expr>, func:LLVMFunction) = 
    match expression with
      |
