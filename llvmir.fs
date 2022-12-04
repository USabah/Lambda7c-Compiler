module lambda7c
open lambda7c
open Recless.Base
open System;
open System.Collections.Generic;
open Option;

//may need to function to translate lltype -> LLVMtype

//fsharpc llvmir.fs -r lambda7c_typing.dll -r recless.dll 

(* Abstract representation of Simplified LLVM IR:  (includes assignment)

LLVM looks like a monster but actually a lot of the stuff you see in a file
produced by   clang -emit-llvm -S   is optional.  Also, not all possible
instructions are as important as others, especially all the possible forms
of the notorious 'getelementptr' instruction. So the abstract representation
types defined here also define a small subset of the LLVM instruction set.
It includes what I believe are the essential instructions that you will need
to write a basic compiler.  We may have to tweak or add a few new cases as
necessary.  The main types are:

LLVMtype : represents LLVM type expressions such as i32
LLVMexpr : expressions can be registers, %r1, or constants 1, etc
Instruction: this is the most important type to study and understand
LLVMdeclaration : for global declarations such as for string literals
BasicBlock : all instructions are in basic blocks, each block has a label
LLVMFunction : represents a function declaration, which is always global
LLVMprogram : top level structure consisting of declarations and functions

There is (currently) only one function in this file: 'destination', which
retrives the destination register of an instruction as an Option<string>.
Not all instructions have destinations.  For example, for %r1 = add i32 ...
the destination is Some("r1").

///////////////// PROGRAMMING ASSIGNMENT: ///////////////////

Write functions to write a LLVMprogram in string form that can be saved to
a file.  Of course this means you must write a function for each type.
For example, for 

  Instruction.Binaryop("r2","add",Basic("i8"),Register("r1"),Iconst(1))

You should return the string
  "%r2 = add i8 %r1, 1\n"
*)

//Definitions
type Vec<'T> = ResizeArray<'T>
type HashMap<'K,'V> = System.Collections.Generic.Dictionary<'K,'V>
type Conslist<'K> = 'K list

type LLVMtype =
  | Basic of string     // i32: Basic("i32")
  | Pointer of LLVMtype //i32*:  Pointer(Basic("i32"))
  | Array_t of int*LLVMtype  // [5 x i8], Array_t(5,Basic("i8"))
  | Userstruct of string
  | Ellipsis  // special case for optional args, like printf(i8*,...)
  | Void_t

type LLVMexpr =
  | Iconst of int
  | Sconst of string
  | Fconst of float
  | I1const of bool  // booleans of type i1 are true and false
  | Register of string   // %r1
  | Global of string     // @str1
  // | Label of string      // not really used
  | Novalue     // default

//to_string helper functions
let rec type_string(t:LLVMtype) = 
  match t with
    | Basic(s) -> s
    | Pointer(t) -> type_string(t) + "*"
    | Array_t(i,t) -> "[" + string(i) + " x "+ type_string(t) + "]"
    | Userstruct(s) -> s /////double check this one
    | Ellipsis -> "..."
    | Void_t -> "void"

let expr_string(expr:LLVMexpr) = 
  match expr with
    | Iconst(i) -> string(i)
    | Fconst(f) -> 
      let s = string(f)
      if s.Contains(".") then s
      else s + ".0"
    | I1const(b) -> string(b).ToLower()
    | Sconst(s) -> s
    | Register(s) -> "%"+s 
    | Global(s) -> "@"+s
    | Novalue -> ""

let aopt_string(alignopt:Option<string>) = 
  match alignopt with
    | Some(s) -> ", " + s
    | None -> ""
  
let philist_string(conslist:Conslist<(LLVMexpr*string)>) = 
  let mutable str = ""
  let mutable expr1 = Novalue
  let mutable bblock = ""
  let last_index = conslist.Length - 1
  let mutable i = 0
  for tup in conslist do
    expr1 <- fst tup
    bblock <- snd tup
    str <- str + "[" + expr_string(expr1) + ", %" + bblock + "]"
    if i <> last_index then
      str <- str + ", "
    i <- i + 1
  str

let opt_reg_string(s:Option<string>) = 
  match s with
    | Some(r) -> "%" + r + " = "
    | None -> ""

let tlist_string(tlist:Conslist<LLVMtype>) = 
  let mutable str = " "
  let last_index = tlist.Length - 1
  if last_index >= 0 then str <- str + "("
  let mutable i = 0
  for t in tlist do
    str <- str + type_string(t)
    if i <> last_index then
      str <- str + ", "
    else str <- str + ")"
    i <- i + 1
  if str = " " then ""
  else str

let tlist_string2(tlist:Vec<LLVMtype>) = 
  let mutable str = " "
  let last_index = tlist.Count - 1
  if last_index >= 0 then str <- str + "("
  let mutable i = 0
  for t in tlist do
    str <- str + type_string(t)
    if i <> last_index then
      str <- str + ", "
    else str <- str + ")"
    i <- i + 1
  if str = " " then ""
  else str

let arglist_string(arglist:Conslist<(LLVMtype*LLVMexpr)>) = 
  let mutable str = "("
  let last_index = arglist.Length - 1
  let mutable i = 0
  for tup in arglist do
    let t = fst tup
    let dest_expr = snd tup
    str <- str + type_string(t) + " " + expr_string(dest_expr)
    if i <> last_index then
      str <- str + ", "
    i <- i + 1
  str + ")"

let func_args_string(arglist:Vec<(LLVMtype*string)>) = 
  let mutable str = "("
  let last_index = arglist.Count - 1
  let mutable i = 0
  for tup in arglist do
    let t = fst tup
    let identifier = snd tup
    str <- str + type_string(t) + " %" + identifier
    if i <> last_index then
      str <- str + ", "
    i <- i + 1
  str + ")"


/////need to come back to this eventually
let structlist_string(vec:Vec<LLVMtype>) = 
  let mutable str = "{ "
  let last_index = vec.Count - 1
  let mutable i = 0
  for t in vec do
    let t_str = type_string(t)
    str <- str + t_str
    if i <> last_index then
      str <- str + ", "
    else str <- str + " }"
    i <- i + 1
  str


type Instruction =  
  // terminator instructions that ends a Basic Block:
  | Ret of LLVMtype*LLVMexpr  // ret i32 4
  | Ret_noval                 // ret
  | Br_uc of string   // unconditional jump: br label %label1
  | Bri1 of LLVMexpr*string*string //always on i1
  // memory ops: store i32 3, i32* %a, optional "align 4", type i32* is omitted
  | Load of string*LLVMtype*LLVMexpr*Option<string>
  | Store of LLVMtype*LLVMexpr*LLVMexpr*Option<string>  //pointer type omitted
  | Alloca of string*LLVMtype*Option<string> // %r1 = alloca i64, align 8
  // ALU ops, always begins with a destination : %r1 = ...
  | Binaryop of string*string*LLVMtype*LLVMexpr*LLVMexpr
  | Unaryop of string*string*Option<string>*LLVMtype*LLVMexpr // fneg float %r1
  // casting operations like %r2 = zext i32 %r1 to i64
  | Cast of string*string*LLVMtype*LLVMexpr*LLVMtype
  // comparison and selection ops, phi
  | Icmp of string*string*LLVMtype*LLVMexpr*LLVMexpr //1st string is dest
  | Fcmp of string*string*LLVMtype*LLVMexpr*LLVMexpr
  | SelectTrue of string*LLVMtype*LLVMexpr*LLVMexpr
  | Phi2 of string*LLVMtype*LLVMexpr*string*LLVMexpr*string
  | Phi of string*LLVMtype*Conslist<(LLVMexpr*string)> //not as common as Phi2
  // function call: option dest,ret type,
  | Call of Option<string>*LLVMtype*Conslist<LLVMtype>*string*Conslist<(LLVMtype*LLVMexpr)>
  // simplified forms of getelementptr for array address and struct field
  | Arrayindex of string*int*LLVMtype*LLVMexpr*LLVMexpr
  | Structfield of string*LLVMtype*LLVMexpr*LLVMexpr
  // BasicBlock instruction
  | BBlock of string
  // other llvm instructions not covered by above must be encoded as:
  | Verbatim of string //generic "other" instruction, default case, comments

  member this.destination() =  
    match this with
      | Load(s,_,_,_) | Alloca(s,_,_) | Unaryop(s,_,_,_,_) -> Some(s)
      | Binaryop(s,_,_,_,_) | Call(Some(s),_,_,_,_) -> Some(s)
      | Cast(s,_,_,_,_) | Icmp(s,_,_,_,_) | Fcmp(s,_,_,_,_) -> Some(s)
      | SelectTrue(s,_,_,_) | Phi2(s,_,_,_,_,_) | Phi(s,_,_) -> Some(s)
      | Arrayindex(s,_,_,_,_) | Structfield(s,_,_,_) -> Some(s) 
      | _ -> None  // includes case for Verbatim
  
  member this.to_string() = 
    let mutable str = ""
    match this with 
      | Ret(t, expr) ->
        str <- "ret " + type_string(t) + " " + expr_string(expr)
        str
      | Ret_noval -> "ret"
      | Br_uc(s) ->
        str <- "br label %" + s 
        str
      | Bri1(expr, s1, s2) ->
        str <- "br i1 " + expr_string(expr) + ", label %" + s1 + ", label %" + s2
        str
      | Load(reg, t, expr, alignopt) ->
        let t_str = type_string(t)
        str <- "%"+reg + " = load " + t_str + ", " + t_str + "* " + expr_string(expr) + aopt_string(alignopt)
        str
      | Store(t, expr1, expr2, alignopt) ->
        let t_str = type_string(t)
        str <- "store " + t_str + " " + expr_string(expr1) + ", " + t_str + "* " + expr_string(expr2) + aopt_string(alignopt)
        str
      | Alloca(reg, t, alignopt) ->
        str <- "%" + reg + " = alloca " + type_string(t) + aopt_string(alignopt)
        str
      | Binaryop(reg, op, t, expr1, expr2) ->
        str <- "%" + reg + " = " + op + " " + type_string(t) + " " + expr_string(expr1) + ", " + expr_string(expr2)
        str
      | Unaryop(reg, op, None, t, expr) -> //only handling None case 
        str <- "%" + reg + " = " + op + " " + type_string(t) + " " + expr_string(expr) 
        str
      | Cast(new_reg, castop, from_t, orig_reg, to_t) ->
        str <- "%" + new_reg + " = " + castop + " " + type_string(from_t) + " " + expr_string(orig_reg) + " to " + type_string(to_t)
        str
      | Icmp(res_reg, op, t, expr1, expr2) ->
        str <- "%" + res_reg + " = icmp " + op + " " + type_string(t) + " " + expr_string(expr1) + ", " + expr_string(expr2)
        str
      | Fcmp(res_reg, op, t, expr1, expr2) ->
        str <- "%" + res_reg + " = fcmp " + op + " " + type_string(t) + " " + expr_string(expr1) + ", " + expr_string(expr2)
        str
      | SelectTrue(reg, t, expr1, expr2) ->
        let t_str = type_string(t)
        str <- "%" + reg + " = select i1 true, " + t_str + " " +  expr_string(expr1) + ", " + t_str + " " + expr_string(expr2)
        str
      | Phi2(reg, t, expr1, block1, expr2, block2) ->
        str <- "%" + reg + " = phi " + type_string(t) + " [" + expr_string(expr1) + ", %" + block1 + "], [" + expr_string(expr2) + ", %" + block2 + "]"
        str
      | Phi(reg, t, conslist) ->
        str <- "%" + reg + " = phi " + type_string(t) + " " + philist_string(conslist)
        str
      | Call(opt_reg, t, typelist, func_name, arglist) ->
        str <- opt_reg_string(opt_reg) + "call " + type_string(t) + tlist_string(typelist) + " @" + func_name + arglist_string(arglist)
        str
      | Arrayindex(reg, index, t, expr1, expr2) ->
        let arrstring = "[" + string(index) + " x " + type_string(t) + "]"
        str <- "%" + reg + " = getelementptr inbounds " + arrstring + ", " + arrstring + "* " + expr_string(expr1) + ", i64 0, i64 " + expr_string(expr2)
        str
      | Structfield(reg, t, expr1, expr2) ->
        let t_str = type_string(t)
        str <- "%" + reg + " = getelementptr inbounds %" + t_str + ", %" + t_str + "* " + expr_string(expr1) + " i32 0, i32 " + expr_string(expr2)
        str
      | BBlock(label) -> label + ":"
      | Verbatim(s) -> s
      | _ -> printfn "OPERATION NOT SUPPORTED: %A" this; ""

(*
The instruction set represented by the F# 'Instruction' type is a simplified
subset of LLVM instructions, encoding only the most commonly used forms that
you will encounter while writing LLVM program by hand, or when generating LLVM
code.  All other LLVM instructions must be encoded with the union-variant
Instruction.Verbatim.

For many of these instructions, information that's most common or obvious
are omitted.  For example, in a conditional branch instruction such as

  br i1 %r1, label %label1, label %label2

The type of of %r1 is almost always i1, and is thus omitted from the abstract
representation. Such an instruction is encoded as

  Instruction.Bri1(LLVMexpr.Register("r1"), "label1", "label2")

In case the type is not 'i1', then the instruction would have to be encoded
as Verbatim("...").

Similar, for a load operation

  %r2 = load i32, i32* %r1  ;no , align4 at the end

The type 'i32*' is omitted since it is derivable from i32.  I don't think
it's even possible to use another type.  The "align 4" is optional. The
encoding of this instruction is

Instruction.Load("r2",LLVMtype.Basic("i32"),LLVMexpr.Register("r1"),None)

Replace None with Some("align 4") if desired.

For instructions that commonly returns a value, the first string is the
destination register:

  %r3 = add i32 %r1, 1

is encoded as

  Instruction.Binaryop("r3","add",Basic("i32"),Register("r1"),Iconst(1))

The Call (function call) instruction has an optional destination, in case
there is no return value for the function being called.  The Call instruction
has another optional component: a list of types that's required for calling
certain types of functions like printf.  The list is empty if no type is
specified.

  %r2 = call i32 @func1(i8* %r1, double 1.0)

is encoded as

  Call(Some("r2"),Basic("i32"),[],"func1",[(Pointer(Basic("i8")),Register("r1")),(Basic("double"),Fconst(1.0))])

The empty list [] represents that no additional type information is needed.
On the other hand, calls to printf requires a a type specifier for printf:

  call i32 (i8*,...) @printf(i8* %r2, i32 %r1)

is encoded as

  Call(None,Basic("i32"),[Pointer(Basic("i8")),Ellipsis],"printf",[(Pointer(Basic("i8")),Register("r2")), (Basic("i32"),Register("r1"))])

The above instruction is probably preceeded by something like:

  %r2 = getelementptr inbounds [9 x i8], [9 x i8]* @str1, i64 0, i64 0

The getelementptr instruction has grown from a relatively easy to understand
operation to an absolute monster.  Only two commonly used forms are included
(others have to be made Verbatim). This above assignment to %r2 is encoded as

  Arrayindex("r2",9,Basic("i8"),Global("str1"),Iconst(0))

This is used to find the address of an array element, so the above is the
address of str1[0] in an array of 9 bytes (char[9]): The last expression
is the array index.  Note that getelementptr always computes an ADDRESS:
it does not extract the value, which must be done with 'load'.

The other form of getelementptr encoded as an Instruction is

  %r2=getelementptr inbounds %bigstruct, %bigstruct* %r1 i32 0, i32 1

This is encoded as

  Structfield("r2",Userstruct("bigstruct"),Register("r1"),Iconst(1))

For example, given struct bigstruct { int x; char y; } b;  &b.y can be
retrieved with this instruction.  Nested struct access (a.b.c) will require
multiple Structfield instructions.

//// Examples of all other instructions (abstract form : concrete form)

Instruction.Ret(LLVMtype.Basic("i32"),LLVMexpr.Register("r1"))  : ret i32 %r1
Ret_noval   :
    ret
Br_uc("loopstart")  :
    br label %loopstart
Bri1(Register("r2"),"label1","label2") :
    br i1 %r2, label %label1, label %label2
Store(Basic("i8"),Iconst(1),Register("r1"),Some("align 1")) :
    store i8 1, i8* %r1, align 1 ; align is not really needed
Alloca("r1",Basic("i32"),Some("align 4")) : %r1 = alloca i32, align 4
    ;alloca allocates on the stack.  Call function malloc for heap allocation
Unaryop("r2","fneg",None,Basic("float"),Register("r1")) : fneg float %r1
    ; fneg is the only unary operation, option is for "fast-math flags"
    ; ... but "fast-math" is not for Hofstra students :-0
Cast("r2","bitcast",Pointer(Basic("i8")),Register("r1"),Pointer(Basic("i32"))):
    %r2 = bitcast i8* %r1 to i32*
Icmp("r2","sle",Basic("i32"),Register("r1"),Iconst(1)) :
    %r2 = icmp sle i32 %r1, 1   ;sle is signed <=
Fcmp("r2","oeq",Basic("float"),Fconst(4.0),Register("r1")) :
    %r2 = fcmp oeq float 4.0, %r1   ;oeq is ordered equality
SelectTrue("r3",Basic("i32"),Register("r1"),Register("r2")) :
    %r3 = select i1 true, i32 %r1, i32 %r2 ;why would you select false
Phi2("r2",Basic("i32"),Iconst(1),"block1",Register("r1"),"block2") :
    %r2 = phi i32 [1, %block1], [%r1, %block2]
Phi("3",Basic("i8"),[(Iconst(1),"bb1"),(Iconst(2),"bb2"),(Iconst(3),"bb3")] :
    %3 = phi i8 [1, %bb1], [2, %bb2], [3, %bb3] ;better to stick to Phi2
BBlock("label") :
    label:
Call(Some("r1"),Basic("i64"),"factorial",[(Basic("i8"),Iconst(8))])
    %r1 = call i64 @factorial(i8 8)
Verbatim("; comments start with ; so you can add ; to end of line")
    
*)

// extracts the destination register from an instruction, returns string option
(*
let destination = function
  | Load(s,_,_,_) | Alloca(s,_,_) | Unaryop(s,_,_,_,_) -> Some(s)
  | Binaryop(s,_,_,_,_) | Call(Some(s),_,_,_,_) -> Some(s)
  | Cast(s,_,_,_,_) | Icmp(s,_,_,_,_) | Fcmp(s,_,_,_,_) -> Some(s)
  | SelectTrue(s,_,_,_) | Phi2(s,_,_,_,_,_) | Phi(s,_,_) -> Some(s)
  | Arrayindex(s,_,_,_,_) | Structfield(s,_,_,_) -> Some(s) 
  | _ -> None  // includes case for Verbatim
*)

type LLVMdeclaration =
  | Globalconst of string*LLVMtype*LLVMexpr*Option<string>
  | Externfunc of LLVMtype*string*Vec<LLVMtype>
  | Structdec of string*Vec<LLVMtype>
  | Verbatim_dec of string // cases not covered by above
  
  member this.to_string() = 
    match this with
      | Globalconst(reg, t, str, alignopt) ->
        sprintf "@%s = constant %s c\"%s\"%s" reg (type_string(t)) (expr_string(str)) (aopt_string(alignopt))
      | Externfunc(t, func_name, tlist) ->
        sprintf "declare %s @%s%s" (type_string(t)) func_name (tlist_string2(tlist))
      | Structdec(struct_name, structlist) ->
        sprintf "%struct.%s = type %s" "%s" struct_name (structlist_string(structlist))
      | Verbatim_dec(s) -> s

(*
   LLVM declarations are special cases of globally scoped instructions.
   LLVM allows forward reference

 Globalconst("str1",Array_t(6,Basic("i8")),Sconst("hello\00"),Some("align 1")) :
    @str1 = constant [6 x i8] c"hello\00", align 1
 Externfunc(Basic("i32"),"printf",[Pointer(Basic("i8")),Ellipsis]) :
    declare i32 @printf(i8*, ...)
 Structdec("bigstruct",[Basic("i32"),Basic("i8"),Basic("double")]) :
    %struct.bigstruct = type { i32, i8, double }
*)


type BasicBlock =
  {
     label: string;
     body: Vec<Instruction>; // last instruction must be a terminator
     predecessors: Vec<string>; //control-flow graph, not used for now
     ssamap: HashMap<string,string>; //current manifestation of each var
  }

  member this.add(inst:Instruction) = 
    this.body.Add(inst)

  member this.append(v:Vec<Instruction>) = 
    this.body.AddRange(v)

  member this.last_dest() = 
    if this.body.Count = 0 then None
    else
      this.body.[this.body.Count-1].destination() 

  member this.add_predecessor(pred:string) = 
    this.predecessors.Add(pred)
  
let newBasicBlock(lb:string, pred:Vec<string>) = 
  { 
      BasicBlock.label=lb; 
      body=Vec<Instruction>(); 
      predecessors=pred; 
      ssamap=HashMap<string,string>(); 
  }

let is_terminator(inst:Instruction) =
  match inst with
    | Br_uc(_) | Bri1(_,_,_) | Ret(_,_) | Ret_noval -> true
    | _ -> false

let is_float(t:LLVMtype) =
  match t with
    | Basic("double") -> true
    | _ -> false 

let oprep(expression:LBox<expr>, isfloat:bool) = 
  match expression with
    | Lbox(Binop(op,_,_)) ->
      match op with
        | "*" -> if isfloat then "fmul" else "mul"
        | "/" -> if isfloat then "fdiv" else "sdiv"
        | "+" -> if isfloat then "fadd" else "add"
        | "-" -> if isfloat then "fsub" else "sub"
        | "%" -> if isfloat then "frem" else "srem"
        | "eq" | "=" -> if isfloat then "oeq" else "eq" 
        | "neq" | "^" -> if isfloat then "one" else "ne" 
        | "<" -> if isfloat then "olt" else "slt" 
        | "<=" -> if isfloat then "ole" else "sle" 
        | ">" -> if isfloat then "ogt" else "sgt" 
        | ">=" -> if isfloat then "oge" else "sge" 
        | "and" when not(isfloat) -> "and" 
        | "or" when not(isfloat) -> "or" 
        | _ -> "INVALID OP"
    | _ -> "INVALID OP"

type LLVMFunction =
  {
     name: string;
     formal_args: Vec<(LLVMtype*string)>;
     return_type: LLVMtype;  // only basic and pointer types can be returned
     body: Vec<BasicBlock>;
     attributes: Vec<string>; // like "dso_local", "#1", or ["","#1"]
     bblocator: HashMap<string,int>; // index of BB in vector by label 
  }

  /////should this add a BB instruction as well? 
  member this.addBB(bb:BasicBlock) = 
    this.bblocator.Add(bb.label, bb.body.Count)
    this.body.Add(bb) |> ignore
    this.add_inst(BBlock(bb.label))

  member this.currentBB(index:int) = 
    let mutable need_new = false
    if this.body.Count = 0 then need_new <- true
    let last = this.body.Count - 1
    if this.body.[last].body.Count > 0 then
      let li = this.body.[last].body.Count - 1
      if is_terminator(this.body.[last].body.[li]) then
        need_new <- true
    if need_new then
      let new_label = sprintf "newBB_%d" index
      let newBB = 
        {
          BasicBlock.label = new_label;
          body = Vec<Instruction>(); // last instruction must be a terminator
          predecessors = Vec<string>(); //control-flow graph, not used for now
          ssamap = HashMap<string,string>(); //current manifestation of each var
        }
      this.addBB(newBB)
      newBB
    else this.body.[last]

  member this.currentBBopt() = 
    if this.body.Count = 0 then None
    else
      let last = this.body.Count - 1
      if this.body.[last].body.Count > 0 then
        let li = this.body.[last].body.Count - 1
        if is_terminator(this.body.[last].body.[li]) then
          None
        else Some(this.body.[last])
      else Some(this.body.[last])

  member this.add_inst(inst:Instruction) = 
    let bb = this.currentBB(0)
    bb.body.Add(inst) |> ignore

  member this.currentBBlabel() = 
    match (this.currentBBopt()) with
      | Some(bb) -> bb.label
      | None -> ""

type LLVMprogram =
  {
     mutable preamble : string;   // arbitrary stuff like target triple
     global_declarations : Vec<LLVMdeclaration>;
     functions: Vec<LLVMFunction>;
     mutable postamble : string;  // stuff you don't want to know about
     strconsts:HashMap<string,string>;
  }

  member this.addGD(decl:LLVMdeclaration) = 
    this.global_declarations.Add(decl)
  
  member this.appendGD(vec:Vec<LLVMdeclaration>) = 
    this.global_declarations.AddRange(vec)

  member this.to_string() = 
    let mutable pr_str = ""
    pr_str <- pr_str + this.preamble + "\n"
    for decl in this.global_declarations do
      pr_str <- pr_str + decl.to_string() + "\n"
    for func in this.functions do
      let func_def_string = 
        sprintf "define %s @%s%s {\n" 
          (type_string(func.return_type)) (func.name) (func_args_string(func.formal_args))
      pr_str <- pr_str + func_def_string 
      for bblock in func.body do
        for instruction in bblock.body do
          pr_str <- pr_str + instruction.to_string() + "\n"
      pr_str <- pr_str + "}\n"
    pr_str <- pr_str + this.postamble 
    pr_str

let newLLVMprogram(name:string) = 
  {
     LLVMprogram.preamble = name; 
     global_declarations = Vec<LLVMdeclaration>();
     functions = Vec<LLVMFunction>();
     postamble = "";
     strconsts = HashMap<string,string>();
  }

//Test cases:
let run_test = 
  let v_dec = Vec<LLVMdeclaration>()
  v_dec.Add(Globalconst("str1",Array_t(6,Basic("i8")),Sconst("hello\00"),Some("align 1")))
  let v_types1 = Vec<LLVMtype>()
  v_types1.Add(Pointer(Basic("i8")))
  v_types1.Add(Ellipsis)
  v_dec.Add(Externfunc(Basic("i32"),"printf",v_types1))
  let v_types2 = Vec<LLVMtype>()
  v_types2.Add(Basic("i32"))
  v_types2.Add(Basic("i8"))
  v_types2.Add(Basic("double"))
  v_dec.Add(Structdec("bigstruct",v_types2))
  let llvmProgram = newLLVMprogram("")
  llvmProgram.appendGD(v_dec)
  let basicBlock = newBasicBlock("", Vec<string>())
  let llvmFunction =
    {
       LLVMFunction.name = "";
       formal_args = Vec<(LLVMtype*string)>();
       return_type = Void_t;  // only basic and pointer types can be returned
       body = Vec<BasicBlock>();
       attributes = Vec<string>(); // like "dso_local", "#1", or ["","#1"]
       bblocator = HashMap<string,int>(); // index of BB in vector by label 
    }
  
  llvmProgram.functions.Add(llvmFunction)
  llvmFunction.body.Add(basicBlock)
  
  basicBlock.body.Add(Instruction.Bri1(LLVMexpr.Register("r1"), "label1", "label2"))
  basicBlock.body.Add(Instruction.Load("r2",LLVMtype.Basic("i32"),LLVMexpr.Register("r1"),None)) 
  basicBlock.body.Add(Instruction.Binaryop("r3","add",Basic("i32"),Register("r1"),Iconst(1))) 
  basicBlock.body.Add(Call(Some("r2"),Basic("i32"),[],"func1",[(Pointer(Basic("i8")),Register("r1"));(Basic("double"),Fconst(1.0))])) 
  basicBlock.body.Add(Call(None,Basic("i32"),[Pointer(Basic("i8"));Ellipsis],"printf",[(Pointer(Basic("i8")),Register("r2"));(Basic("i32"),Register("r1"))])) 
  basicBlock.body.Add(Arrayindex("r2",9,Basic("i8"),Global("str1"),Iconst(0))) 
  basicBlock.body.Add(Structfield("r2",Userstruct("bigstruct"),Register("r1"),Iconst(1))) 
  basicBlock.body.Add(Instruction.Ret(LLVMtype.Basic("i32"),LLVMexpr.Register("r1"))) 
  basicBlock.body.Add(Instruction.Ret_noval) 
  basicBlock.body.Add(Instruction.Br_uc("loopstart")) 
  basicBlock.body.Add(Instruction.Bri1(Register("r2"),"label1","label2")) 
  basicBlock.body.Add(Instruction.Store(Basic("i8"),Iconst(1),Register("r1"),Some("align 1"))) 
  basicBlock.body.Add(Instruction.Alloca("r1",Basic("i32"),Some("align 4"))) 
  basicBlock.body.Add(Instruction.Unaryop("r2","fneg",None,Basic("float"),Register("r1"))) 
  basicBlock.body.Add(Instruction.Cast("r2","bitcast",Pointer(Basic("i8")),Register("r1"),Pointer(Basic("i32")))) 
  basicBlock.body.Add(Instruction.Icmp("r2","sle",Basic("i32"),Register("r1"),Iconst(1))) 
  basicBlock.body.Add(Instruction.Fcmp("r2","oeq",Basic("float"),Fconst(4.0),Register("r1"))) 
  basicBlock.body.Add(Instruction.SelectTrue("r3",Basic("i32"),Register("r1"),Register("r2"))) 
  basicBlock.body.Add(Instruction.Phi2("r2",Basic("i32"),Iconst(1),"block1",Register("r1"),"block2")) 
  basicBlock.body.Add(Instruction.Phi("3",Basic("i8"),[(Iconst(1),"bb1");(Iconst(2),"bb2");(Iconst(3),"bb3")])) 
  basicBlock.body.Add(Instruction.Call(Some("r1"),Basic("i64"),[],"factorial",[(Basic("i8"),Iconst(8))])) 
  basicBlock.body.Add(Instruction.Verbatim("; comments start with ; so you can add ; to end of line")) 
  basicBlock.body.Add(Instruction.BBlock("label")) 
  
  let str = llvmProgram.to_string() //for now
  printfn "%s" str

run_test
