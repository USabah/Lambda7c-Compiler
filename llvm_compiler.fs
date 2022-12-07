module lambda7c
open System
open System.Collections.Generic
open lambda7c
open Recless.Base
open Option

type Conslist<'K> = 'K list

//fsharpc llvm_compiler.fs -r llvmir.dll

let cmp_op = [">";">=";"eq";"=";"neq";"^";"<";"<="]
let short_circuit_op = ["and";"&&";"or";"||"]

//Compiler
type LLVMCompiler =
  {
    symbol_table: SymbolTable;
    program: LLVMprogram;  // to be built
    mutable gindex: int; // global counter
    mutable lindex: int; // local counter, set to 0 prior to compiling functions
    mutable errors: bool;
    allocated_vars: HashSet<string>;
    // maps each closure to its table frame and a hashmap that maps each field to its position and type 
    // Hash(closure_var, (frame, Hash(field, (pos, type))))
    clsmaps: HashMap<string,(table_frame*HashMap<string,(int*LLVMtype)>)>; 
  } //LLVMCompiler

  member this.newid(str:string) = 
    this.lindex <- this.lindex + 1
    let new_id = sprintf "%s_%d" str (this.lindex)
    new_id
  
  member this.newgid(str:string) = 
    this.gindex <- this.gindex + 1
    let new_id = sprintf "%s_%d" str (this.gindex)
    new_id
  
  member this.oldgid(str:string) = 
    let old_id = sprintf "%s_%d" str (this.gindex)
    old_id
  
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
      | Lbox(Strlit(s)) ->
        //calculate size of s, add necessary string stuff like \0a\00
        let mutable str = s
        let mutable strid = ""
        let mutable str_size = str.Length
        let new_line_count = str.Split("\\n").Length - 1
        str <- str.Replace("\\n","\\0a") 
        str_size <- str_size - new_line_count
        str <- str + "\\00"
        str_size <- str_size + 1
        if this.program.strconsts.ContainsKey(str) then
          strid <- this.program.strconsts.[str]
        else
          strid <- this.newgid("str")
          let decl = Globalconst(strid,Array_t(str_size,Basic("i8")),Sconst(str),None)
          this.program.addGD(decl)
          this.program.strconsts.Add(str, strid)
        let reg = this.newid("r")
        let arr_inst = Arrayindex(reg,str_size,Basic("i8"),Global(strid),Iconst(0))
        func.add_inst(arr_inst)
        Register(reg)
      | Lbox(Nil) -> Novalue
      | Lbox(Binop(op,a,b)) when List.contains op cmp_op ->
        printfn "IN BINOP"
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
        let r2 = this.newid("r")
        //Need to convert to i32 since Ifelse and Whileloop assume that it's 
        //an i32
        func.add_inst(Cast(r2,"zext",Basic("i1"),Register(r1),Basic("i32")))
        Register(r2)
      //| Lbox(Binop("cons",a,b)) ->
      | Lbox(Binop("and",a,b)) | Lbox(Binop("&&",a,b)) ->
          printfn "IN BINOP"
          let sc_lbox = lbox("", Integer(0), expression.line, expression.column)
          let ifelse_expr_box = lbox("", Ifelse(a,b,sc_lbox), expression.line, expression.column)
          this.compile_expr(ifelse_expr_box, func) 
      | Lbox(Binop("or",a,b)) | Lbox(Binop("||",a,b)) ->
          printfn "IN BINOP"
          let sc_lbox = lbox("", Integer(1), expression.line, expression.column)
          let ifelse_expr_box = lbox("", Ifelse(a,sc_lbox,b), expression.line, expression.column)
          this.compile_expr(ifelse_expr_box, func) 
      | Lbox(Binop(op,a,b)) ->
        printfn "IN BINOP"
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
      | Lbox(Uniop("display", exp)) ->
        printfn "IN UNIOP"
        let dest = this.compile_expr(exp, func)
        let exptype = this.symbol_table.infer_type(exp)
        match exptype with
          | LLint -> //Basic("i32") 
            let call_inst = Call(None,Void_t,[],"lambda7c_printint",[(Basic("i32"),dest)])
            func.add_inst(call_inst)
            Novalue
          | LLfloat-> // Basic("double") ->
            let call_inst = Call(None,Void_t,[],"lambda7c_printfloat",[(Basic("double"),dest)])
            func.add_inst(call_inst)
            Novalue
          | LLstring -> //Array_t
            ////this.newgid's most recent return the string we're printing?
            let strid = this.oldgid("str")
            let call_inst = Call(None,Void_t,[],"lambda7c_printstr",[(Pointer(Basic("i8")),dest)])
            func.add_inst(call_inst)
            Novalue
          | _ -> 
            printfn "(%d,%d): COMPILER ERROR: Expression is not yet supported by the compiler"
              expression.line expression.column
            this.errors <- true
            Novalue
      | Lbox(Ifelse(cond,tcase,fcase)) ->
        printfn "IN IFELSE"
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
      
      | Lbox(Define(Lbox(_,var),(Lbox(TypedLambda(args,_,exp)) as l_expr)))
      | Lbox(Define(Lbox(_,var),(Lbox(Lambda(args,exp)) as l_expr))) ->
        printfn "IN LAMBDA DEFINE %s" var
        let orig_lindex = this.lindex
        this.lindex <- 0
        let fun_type = this.symbol_table.infer_type(expression)
        let r_type = 
          this.translate_type(
            match fun_type with
              | LLfun(_,r) -> r
              | _ -> LLuntypable
          )
        //let r_type = this.translate_type(this.symbol_table.infer_type(expression))
        //swap frames
        let func_frame = this.symbol_table.frame_hash.[(l_expr.line,l_expr.column)]
        let orig_frame = this.symbol_table.current_frame
        this.symbol_table.current_frame <- func_frame

        let argsVec = Vec<LLVMtype*string>() //args + closure
        let mutable llvm_arg_type = Void_t
        let entryopt = this.symbol_table.get_entry(var,0)
        let var_entry = entryopt.Value
        let gindex =
          match var_entry with
            | LambdaDef(_,g,_,_) -> g
            | _ -> //SHOULDN'T BE REACHABLE
              printfn "UNREACHABLE"
              0
        let fun_identifier = sprintf "%s_%d" var gindex
        let lambda_fun = 
          {
            LLVMFunction.name = fun_identifier;
            formal_args = argsVec;
            return_type = r_type;
            body = Vec<BasicBlock>();
            attributes = Vec<string>();
            bblocator = HashMap<string,int>();
          }
        lambda_fun.addBB(newBasicBlock("beginfun", Vec<string>()))
        
        //Add function arguments
        for Lbox(argTup) in args do
          /////////////////////don't load original identifier, load new identifier
          let (spec_type,arg) = argTup
          let entryopt = this.symbol_table.get_entry(arg,0)
          let var_entry = entryopt.Value
          let gindex =
            match var_entry with
              | LambdaDef(_,g,_,_) -> g
              | SimpleDef(_,g,_) -> g
          let farg_identifier = sprintf "farg_%s_%d" arg gindex
          if isSome spec_type then
            llvm_arg_type <- this.translate_type(spec_type.Value)
          else 
            llvm_arg_type <- this.translate_type(this.symbol_table.get_type(arg,0))
          argsVec.Add((llvm_arg_type, farg_identifier))
        
        //Add initialization instructions
        for (f_arg_type, f_arg_name) in argsVec do
          let arg_name = f_arg_name.Substring(5)
          let allocainst = Alloca(arg_name, f_arg_type, None)
          let storeinst = Store(f_arg_type, Register(f_arg_name), Register(arg_name), None)
          lambda_fun.add_inst(allocainst) 
          lambda_fun.add_inst(storeinst) 

        //Add closure arguments
        let closure = func_frame.closure
        for kvPair in closure do
          llvm_arg_type <- Pointer(this.translate_type(kvPair.Value))
          let (var_name, gindex) = kvPair.Key
          let free_var_identifier = sprintf "%s_%d" var_name gindex
          argsVec.Add((llvm_arg_type, free_var_identifier))  
        
        this.lindex <- orig_lindex
        let reg = this.compile_expr(exp, lambda_fun)
        //restore original frame
        this.symbol_table.current_frame <- orig_frame
        let ret = Ret(r_type, reg) /////double check if reg is the value
        lambda_fun.add_inst(ret)
        this.program.functions.Add(lambda_fun)
        reg
     
      | Lbox(Define(Lbox(_,var), value)) | Lbox(TypedDefine(Lbox(_,var), value))
      | Lbox(Setq(Lbox(var), value)) ->
        printfn "IN DEFINE/SETQ"
        let identifier = var
        let entryopt = this.symbol_table.get_entry(identifier,0)
        let var_entry = entryopt.Value
        let (etype, gindex) =
          match var_entry with
            | SimpleDef(l,g,_) -> (l,g)
            | LambdaDef(l,g,_,_) -> (l,g)
        let var_str = sprintf "%s_%d" identifier gindex
        let expr_dest = this.compile_expr(value, func)
        let mutable desttype = Void_t 
        if etype = LLstring then
          desttype <- Pointer(Basic("i8"))
        else 
          desttype <- this.translate_type(etype)
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
        printfn "IN VAR: %s" x
        printfn "FRAME_NAME: %s" (this.symbol_table.current_frame.name)
        let entryopt = this.symbol_table.get_entry(x,0)
        ///////////ENTRYOPT IS NONE, IN THE WRONG FRAME (FRAME AREA)
        let var_entry = entryopt.Value
        let (etype, gindex) = 
          match var_entry with
            | SimpleDef(l,g,_) -> (l,g)
            | LambdaDef(l,g,_,_) -> (l,g)
        let var_str = sprintf "%s_%d" x gindex
        let mutable desttype = Void_t
        if etype = LLstring then
          desttype <- Pointer(Basic("i8"))
        else
          desttype <- this.translate_type(etype)
        let reg = this.newid("r") 
        let loadinst = Load(reg,desttype,Register(var_str),None)
        func.add_inst(loadinst)
        Register(reg)
      | Lbox(Sequence(Lbox(Var("getint"))::args)) when args.Length=0 ->
        printfn "IN GETINT"
        let r1 = this.newid("in")
        func.add_inst(Call(Some(r1),Basic("i32"),[],"lambda7c_cin",[]))
        Register(r1)
      | Lbox(Sequence(Lbox(Var(func_name))::args as lb)) ->
        printfn "FUNCTION APPLICATION %s" func_name
        //Function application OR return var
        let entry_opt = this.symbol_table.get_entry(func_name,0)
        let entry = entry_opt.Value
        let (typeList, return_type, func_frame_opt, global_index) =  
          match entry with
            | LambdaDef(t,g,f,_) -> 
              match t with
                | LLfun(l,r) -> (l,r,Some(f), g)
                | _ -> ([LLuntypable], LLuntypable, None, 0) //Should be unreachable...
            | SimpleDef(t,g,a) -> ([t], LLuntypable, None, 0) 
        if global_index = 0 then
          let t = this.translate_type(typeList.[0])
          let reg = this.compile_expr(lb.[0],func)
          printfn "HERE"
          let ret_inst = Ret(t, reg)
          reg 
        else
          let func_identifier = sprintf "%s_%d" func_name global_index
          let func_frame = func_frame_opt.Value
          let closure = func_frame.closure
          
          let mutable argtypeList = [(Void_t, Novalue)]
          argtypeList <-
            match argtypeList with
              | a::b -> b
              | a -> a
       
          //Add closure args (in reverse)
          for kvPair in closure do
            let llvm_arg_type = Pointer(this.translate_type(kvPair.Value))
            let (var_name, gindex) = kvPair.Key
            let reg_str = sprintf "%s_%d" var_name gindex
            let exp = Register(reg_str)
            argtypeList <- (llvm_arg_type, exp)::argtypeList
          
          let mutable func_index = args.Length - 1;
          //Add function args (in reverse, due to list data structure) 
          while func_index >= 0 do
            let exp = args.[func_index]
            let arg = exp.value
            let argtype = this.translate_type(typeList.[func_index])
            let dest = this.compile_expr(exp,func)
            argtypeList <- (argtype,dest)::argtypeList
            func_index <- func_index - 1
          
          //swap frames
          let orig_frame = this.symbol_table.current_frame
          this.symbol_table.current_frame <- func_frame
          
          let desttype = this.translate_type(return_type) 
          let mutable reg = Some("")
          if desttype <> Void_t then
            let reg_name = this.newid("r")
            reg <- Some(reg_name)
          else
            reg <- None
          func.add_inst(Call(reg,desttype,[],func_identifier,argtypeList))
          //restore frame
          this.symbol_table.current_frame <- orig_frame
          if isSome reg then
            Register(reg.Value)
          else
            Novalue
      | Lbox(Let(Lbox(var_tupl), value, exp)) | Lbox(TypedLet(Lbox(var_tupl), value, exp)) ->
        printfn "IN LET"
        let value_reg = this.compile_expr(value,func)
        let func_frame = this.symbol_table.frame_hash.[(expression.line,expression.column)]
        let (_,var_name) = var_tupl
        let entry = func_frame.entries.[var_name]
        let (var_type_LL, global_index) = 
          match entry with
            | SimpleDef(l,g,_) -> (l,g)
            | LambdaDef(l,g,_,_) -> (l,g) //shouldn't be reachable (for now)
        let var_type = this.translate_type(var_type_LL)
        let var_reg_name = sprintf "%s_%d" var_name global_index
        //should compile similar to define
        func.add_inst(Alloca(var_reg_name, var_type, None))
        func.add_inst(Store(var_type, value_reg, Register(var_reg_name), None))
        //swap frames
        let orig_frame = this.symbol_table.current_frame
        this.symbol_table.current_frame <- func_frame
        let body_reg = this.compile_expr(exp,func)
        //restore frame
        this.symbol_table.current_frame <- orig_frame 
        body_reg
      | Lbox(Sequence(se)) | Lbox(Beginseq(se)) ->
        printfn "IN SEQUENCE"
        let mutable ddest = Register("r1")
        for expres in se do
          ddest <- this.compile_expr(expres,func)
        ddest
      | Lbox(Whileloop(cond,loop)) ->
        printfn "IN LOOP"
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
        let ccast = this.newid("r")
        //cdest will be of type i32, not i1 because of lambda7c booleans
        //need to downcast cdest to an i1 before branch
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
        printfn "EXPRESSION::: %A" expression
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
      | LLuntypable -> None //errors handled by typechecker
      | _ ->
        ///////////////////this should be in sym_table.global_declarations
        ///////////////////except target triple
        this.program.preamble <- sprintf "%s"
          "target triple = \"x86_64-pc-linux-gnu\""
        let gdecVec = Vec<LLVMdeclaration>()
        gdecVec.Add(Externfunc(Void_t, "lambda7c_printint", Vec<LLVMtype>([Basic("i32")])))
        gdecVec.Add(Externfunc(Void_t, "lambda7c_printfloat", Vec<LLVMtype>([Basic("double")])))
        gdecVec.Add(Externfunc(Void_t, "lambda7c_printstr", Vec<LLVMtype>([Pointer(Basic("i8"))])))
        gdecVec.Add(Externfunc(Void_t, "lambda7c_newline", Vec<LLVMtype>()))
        gdecVec.Add(Externfunc(Basic("i32"), "lambda7c_cin", Vec<LLVMtype>()))
        this.program.appendGD(gdecVec)

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
          Some(pstr)
        else None

let new_skeleton(name:string) = 
  {
    LLVMCompiler.symbol_table = symbol_table; //defined in lambda7c_typing.fs
    program = newLLVMprogram(name);
    gindex = 0;
    lindex = 0;
    errors = false;
    allocated_vars = HashSet<string>();
    clsmaps = HashMap<string,(table_frame*HashMap<string,(int*LLVMtype)>)>(); 
  }

let llvm_compiler = new_skeleton("") 
