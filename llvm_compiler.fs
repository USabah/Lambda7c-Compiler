module lambda7c
open System
open System.Collections.Generic
open lambda7c
open Recless.Base
open Option

//To start off, do the following cases: Add, 

//fsharpc llvm_compiler.fs -r llvmir.dll

//Compiler
type LLVMCompiler =
{
  symbol_table : SymbolTable,
  program: LLVMProgram,  // to be built
  gindex: int, // global counter
  lindex: int, // local counter, set to 0 prior to compiling functions
  // .. other stuff omitted
} //LLVMCompiler


  member this.newid(str:string) = 
    this.lindex <- lindex + 1
    let new_id = sprintf "%s_%d" str (this.lindex)
    new_id

  member this.translate_type(expr_t:lltype) =
    match expr_t with
      | LLint -> Basic("i32")
      | LLfloat -> Basic("double")
      //| LLstring -> 
      //| LList(a) -> Arr
      | _ -> Novalue

  this.translate_comparison(op:string) = 
    match op with
      | ">" -> "ugt" 
      | ">=" -> "uge" 
      | "eq" | "=" -> "eq"
      | "neq" | "^" -> "ne"
      | "<" -> "ult"
      | "<=" -> "ule"

  //returns the destination register of the expression (of type LLVMexpr)
  member this.compile_expr(expression:LBox<expr>, func:LLVMFunction) = 
    match expression with
      | Lbox(Integer(i)) -> Iconst(i)
      | Lbox(Floatpt(f)) -> Fconst(f)
      | Lbox(Strlit(s)) -> 

      ///////Sconst(s)
      | Lbox(Nil) -> Novalue
      | Lbox(Binop("+",a,b)) ->
        let desta = this.compile_expr(a)
        let destb = this.compile_expr(b)
        let r1 = this.new_id("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        match rtype with
          | Basic("double") -> 
            func.add_inst(Binaryop(r1,"fadd",rtype,desta,destb))
          | _ -> 
            func.add_inst(Binaryop(r1,"add",rtype,desta,destb))
        Register(r1)
      | Lbox(Binop("-",a,b)) ->
        let desta = this.compile_expr(a)
        let destb = this.compile_expr(b)
        let r1 = this.new_id("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        match rtype with
          | Basic("double") -> 
            func.add_inst(Binaryop(r1,"fsub",rtype,desta,destb))
          | _ -> 
            func.add_inst(Binaryop(r1,"sub",rtype,desta,destb))
        Register(r1)
      | Lbox(Binop("*",a,b)) ->
        let desta = this.compile_expr(a)
        let destb = this.compile_expr(b)
        let r1 = this.new_id("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        match rtype with
          | Basic("double") -> 
            func.add_inst(Binaryop(r1,"fmul",rtype,desta,destb))
          | _ -> 
            func.add_inst(Binaryop(r1,"mul",rtype,desta,destb))
        Register(r1)
      | Lbox(Binop("/",a,b)) ->
        let desta = this.compile_expr(a)
        let destb = this.compile_expr(b)
        let r1 = this.new_id("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        match rtype with
          | Basic("double") -> 
            func.add_inst(Binaryop(r1,"fdiv",rtype,desta,destb))
          | _ -> 
            func.add_inst(Binaryop(r1,"udiv",rtype,desta,destb))
        Register(r1)
      | Lbox(Binop("%",a,b)) ->
        let desta = this.compile_expr(a)
        let destb = this.compile_expr(b)
        let r1 = this.new_id("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        match rtype with
          | Basic("double") -> 
            func.add_inst(Binaryop(r1,"frem",rtype,desta,destb))
          | _ -> 
            func.add_inst(Binaryop(r1,"urem",rtype,desta,destb))
        Register(r1)
      | Lbox(Binop("and",a,b)) ->
        let desta = this.compile_expr(a)
        let destb = this.compile_expr(b)
        let r1 = this.new_id("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        func.add_inst(Binaryop(r1,"and",rtype,desta,destb))
        Register(r1)
      | Lbox(Binop("or",a,b)) ->
        let desta = this.compile_expr(a)
        let destb = this.compile_expr(b)
        let r1 = this.new_id("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        func.add_inst(Binaryop(r1,"or",rtype,desta,destb))
        Register(r1)
      //| Lbox(Binop("cons",a,b)) ->
      | Lbox(Binop(cmp,a,b)) ->
        let desta = this.compile_expr(a)
        let destb = this.compile_expr(b)
        let r1 = this.new_id("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        let cmp_op = this.translate_comparison(cmp)
        match rtype with
          | Basic("double") -> 
            func.add_inst(Fcmp(r1,cmp_op,rtype,desta,destb))
          | _ -> 
            func.add_inst(Icmp(r1,cmp_op,rtype,desta,destb))
        Register(r1)
      | Lbox(Uniop())
      //compile Var and Setq cases


let new_skeleton(name:string) = 
  {
    LLVMCompiler.symbol_table = SymbolTable();
    program = newLLVMProgram(name);
    gindex = 0;
    lindex = 0;
  }
