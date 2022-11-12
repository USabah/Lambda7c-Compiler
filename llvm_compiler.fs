module lambda7c
open System
open System.Collections.Generic
open lambda7c
open Recless.Base
open Option

//To start off, do the following cases: Add, 

//fsharpc llvm_compiler.fs -r llvmir.dll

let cmp_op = [">";">=";"eq";"=";"neq";"^";"<";"<="]

//Compiler
type LLVMCompiler =
  {
    symbol_table: SymbolTable;
    program: LLVMprogram;  // to be built
    mutable gindex: int; // global counter
    mutable lindex: int; // local counter, set to 0 prior to compiling functions
    mutable errors: bool;
    allocated_vars: HashSet<string>;
    // .. other stuff omitted
  } //LLVMCompiler

  member this.newid(str:string) = 
    this.lindex <- this.lindex + 1
    let new_id = sprintf "%s_%d" str (this.lindex)
    new_id

  member this.translate_type(expr_t:lltype) =
    match expr_t with
      | LLint -> Basic("i32")
      | LLfloat -> Basic("double")
      //| LLstring -> 
      //| LList(a) -> Arr
      | _ -> Void_t

  //member this.add_BB();;;; to see if lindex+=1 

  //returns the destination register of the expression (of type LLVMexpr)
  member this.compile_expr(expression:LBox<expr>, func:LLVMFunction) = 
    match expression with
      | Lbox(Integer(i)) -> Iconst(i)
      | Lbox(Floatpt(f)) -> Fconst(f)
      /////| Lbox(Strlit(s)) -> 
      | Lbox(Nil) -> Novalue
      | Lbox(Binop(op,a,b)) when List.contains op cmp_op ->
        let desta = this.compile_expr(a, func)
        let destb = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        match rtype with
          | Basic("double") -> 
            let cmp_op = oprep(expression, true)
            func.add_inst(Fcmp(r1,cmp_op,rtype,desta,destb))
          | _ -> 
            let cmp_op = oprep(expression, false)
            func.add_inst(Icmp(r1,cmp_op,rtype,desta,destb))
        Register(r1)
      //| Lbox(Binop("cons",a,b)) ->
      | Lbox(Binop(op,a,b)) ->
        let desta = this.compile_expr(a, func)
        let destb = this.compile_expr(b, func)
        let r1 = this.newid("r")
        let rtype = this.translate_type(this.symbol_table.infer_type(expression))
        match rtype with
          | Basic("double") -> 
            let new_op = oprep(expression, true)
            func.add_inst(Binaryop(r1,new_op,rtype,desta,destb))
          | _ -> 
            let new_op = oprep(expression, false)
            func.add_inst(Binaryop(r1,new_op,rtype,desta,destb))
        Register(r1)
      //| Lbox(Uniop()) ->
      | Lbox(Ifelse(cond,tcase,fcase)) ->
        let cdest = this.compile_expr(cond, func)
        //cdest will be of type i32, not i1 because of lambda7c booleans
        //need to downcast cdest to an i1 before branch
        let ccast = this.newid("r")
        func.add_inst(Cast(ccast,"trunc",Basic("i32"),cdest,Basic("i1")))
        let label1 = this.newid("iftrue")
        let label0 = this.newid("iffalse")
        let endif = this.newid("endif")
        let brinst = Bri1(Register(ccast), label1, label0)
        let predlabel = func.currentBBlabel()
        let v_ifelse = Vec<string>()
        v_ifelse.Add(predlabel) ///should lindex be in the name or bbindex counter???
        func.add_inst(brinst) //add to current BB of function
        //this basic block is now complete and already inside func, and a new BB was created
        let v_endifBB = Vec<string>()
        //True Case Block
        let BB1 = newBasicBlock(label1, v_ifelse)
        func.addBB(BB1)
        let dest1 = this.compile_expr(tcase, func)
        //this could terminate BB1 and create more BBs ...
        let realabel1 = func.currentBBlabel() //must call before termination
        let v_true = Vec<string>()
        v_true.Add(realabel1) //must call before termination
        v_endifBB.Add(realabel1)
        func.add_inst(Br_uc(endif)) //currentBB terminated
        //False Case Block
        let BB0 = newBasicBlock(label0, v_ifelse) /////should this be v_ifelse instead of v_true? i believe so
        func.addBB(BB0)
        let dest0 = this.compile_expr(fcase, func)
        let realabel0 = func.currentBBlabel() //must call before termination
        let v_false = Vec<string>()
        v_false.Add(realabel0)
        v_endifBB.Add(realabel0)
        func.add_inst(Br_uc(endif)) //terminated
        
        //each compile_expr should leave the last BB open
        //EndIf Block
        let newBB = newBasicBlock(endif, v_endifBB)
        func.addBB(newBB)
        //check type of true branch (same as false branch)
        let desttype = this.translate_type(this.symbol_table.infer_type(tcase))
        match desttype with
          | Void_t -> Novalue
          | _ -> 
            let fdest = this.newid("r")
            let phiinst = Phi2(fdest,desttype,dest1,realabel1,dest0,realabel0)
            func.add_inst(phiinst)
            Register(fdest)
      | Lbox(Define(Lbox(_,var), value)) | Lbox(Setq(Lbox(var), value)) ->
        let identifier = var
        let entryopt = this.symbol_table.get_entry(identifier,0)
        let var_entry = entryopt.Value
        let (etype, gindex) =
          match var_entry with
            | SimpleDef(l,g,_) -> (l,g)
            | LambdaDef(l,g,_,_) -> (l,g)
        let var_str = sprintf "%s_%d" identifier gindex
        let desttype = this.translate_type(etype)
        let expr_dest = this.compile_expr(value, func)
        let already_allocated = this.allocated_vars.Contains(var_str)
        /////printfn "VAR_STR: %s" var_str //as of now, two defines of the same variable
        //which are in the same scope will use the newly assigned gindex
        if not(already_allocated) then
          this.allocated_vars.Add(var_str) |> ignore
          let allocainst = Alloca(var_str, desttype, None)
          func.add_inst(allocainst)
        let storeinst = Store(desttype, expr_dest, Register(var_str), None)
        func.add_inst(storeinst)
        Register(var_str)
      | Lbox(Var(x)) ->
        let entryopt = this.symbol_table.get_entry(x,0)
        let var_entry = entryopt.Value
        let (etype, gindex) = 
          match var_entry with
            | SimpleDef(l,g,_) -> (l,g)
            | LambdaDef(l,g,_,_) -> (l,g)
        let var_str = sprintf "%s_%d" x gindex
        let desttype = this.translate_type(etype)
        let reg = this.newid("r") /////not var_str
        let loadinst = Load(reg,desttype,Register(var_str),None)
        func.add_inst(loadinst)
        Register(reg)
      | Lbox(Sequence(se)) | Lbox(Beginseq(se)) ->
        let mutable ddest = Register("r1")
        for expres in se do
          ddest <- this.compile_expr(expres,func)
        ddest
      /////loop and compile cond
      | Lbox(Whileloop(cond,loop)) ->
        let label_cond = this.newid("check_cond")
        let label_loop = this.newid("loop")
        let label_endloop = this.newid("endloop")
        let predlabel = func.currentBBlabel()
        let v_pred = Vec<string>()
        v_pred.Add(predlabel)
        func.add_inst(Br_uc(label_cond)) //end of pred block
        //Condition Block
        let v_endloop = Vec<string>()
        let BBCond = newBasicBlock(label_cond, v_pred)
        func.addBB(BBCond)
        let cdest = this.compile_expr(cond, func)
        //cdest will be of type i32, not i1 because of lambda7c booleans
        //need to downcast cdest to an i1 before branch
        let ccast = this.newid("r")
        func.add_inst(Cast(ccast,"trunc",Basic("i32"),cdest,Basic("i1")))
        let brinst = Bri1(Register(ccast), label_loop, label_endloop)
        let realabel_cond = func.currentBBlabel()
        let v_cond = Vec<string>()
        v_cond.Add(realabel_cond)
        v_endloop.Add(realabel_cond)
        func.add_inst(brinst) //end of condition block
        //Loop Block
        let BBLoop = newBasicBlock(label_loop, v_cond)
        func.addBB(BBLoop)
        let valdest = this.compile_expr(loop, func)
        let realabel_loop = func.currentBBlabel()
        let v_loop = Vec<string>()
        v_loop.Add(realabel_loop)
        func.add_inst(Br_uc(label_cond)) //end of loop block
        //since it goes to label_cond, add the realabel_loop as a predecessor
        v_pred.AddRange(v_loop)
        
        //Endloop Block
        let BBEndloop = newBasicBlock(label_endloop, v_endloop)
        func.addBB(BBEndloop)
        //Whileloop returns type LLunit, so Novalue is returned
        Novalue
      | _ -> 
        printfn "(%d,%d): COMPILER ERROR: Expression is not yet supported by the compiler"
          expression.line expression.column
        this.errors <- true
        Novalue

      //compile Var and Setq cases

  member this.compile_program(mainexpr:LBox<expr>) = 
    //type check and build symbol table:
    printfn "\n\n\nType Checker--------------------"
    let ptype = this.symbol_table.infer_type(mainexpr)
    printfn "Type returned: %A" ptype
    match ptype with
      | LLuntypable -> "" //errors handled by typechecker
      | _ ->
        this.program.preamble <- sprintf "%s\n%s\n%s\n%s\n%s"
          "target triple = \"x86_64-pc-linux-gnu\"; ... other stuff"
          "declare void @lambda7c_printint(i32)"
          "declare void @lambda7c_printfloat(double)"
          "declare void @lambda7c_printstr(i8*)"
          "declare void @lambda7c_newline(i8*)"
          
        //create a main function, but don't push onto program until end
        let mainfunc = 
          {
            LLVMFunction.name = "main";
            formal_args = Vec<(LLVMtype*string)>();
            return_type = Basic("i32");
            body = Vec<BasicBlock>();
            attributes = Vec<string>();
            bblocator = HashMap<string,int>();
          }
        //add initial basic block to main:
        mainfunc.addBB(newBasicBlock("beginmain", Vec<string>()))

        //compile expression sequence (lambda7c AST)
        let mainres = this.compile_expr(mainexpr, mainfunc)
        if not(this.errors) then
          //generate "return 0;" instruction at the end of main
          let ret = Ret(Basic("i32"),Iconst(0))
          mainfunc.add_inst(ret) //terminates the last basic block
          this.program.functions.Add(mainfunc) //add function to program
          
          let pstr = this.program.to_string() //convert to string format (actual .ll form)
          printfn "\n\n\nCode Generation-----------------"
          printfn "%s" pstr
          pstr
        else ""

let new_skeleton(name:string) = 
  {
    LLVMCompiler.symbol_table = symbol_table; //defined in lambda7c_typing.fs
    program = newLLVMprogram(name);
    gindex = 0;
    lindex = 0;
    errors = false;
    allocated_vars = HashSet<string>();
  }

let llvm_compiler = new_skeleton("") 
