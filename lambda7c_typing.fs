module lambda7c
open System
open System.Collections.Generic
open lambda7c
open Recless.Base
open Option

//fsharpc lambda7c_typing.fs -r lambda7c_ast.dll

//lltype Methods
let rec grounded_type = function    // equiv. to match arg with ...
  | LLunknown | LLvar(_) | LLuntypable -> false
  | LList(t) -> grounded_type t
  | lltype.LLtuple(vs) -> List.forall grounded_type vs
  | LLfun(args,rtype) ->  List.forall grounded_type (rtype::args)
  | _ -> true

let numerical = function
  | LLint | LLfloat -> true
  | _ -> false 

let funtype = function
  | LLfun(_) -> true
  | _ -> false

//Symbol Table

type TableEntry =
  | SimpleDef of typeof:lltype * gindex:int * ast_rep:option<expr>
  | LambdaDef of typeof:lltype *gindex:int * frame:table_frame * ast_rep:option<expr>
and table_frame =
  {
    name : string;
    entries:HashMap<string, TableEntry>;
    parent_scope:table_frame option;
  }

type SymbolTable =  // wrapping structure for symbol table frames
  {
     mutable current_frame: table_frame;
     mutable global_index: int;
     frame_hash:HashMap<(int*int),table_frame>;
  }
  
  member this.add_entry(s:string,t:lltype,a:expr option) = 
    if not(this.current_frame.entries.ContainsKey(s)) then
      this.global_index <- this.global_index + 1
      this.current_frame.entries.Add(s,SimpleDef(t, this.global_index, a))
      this.global_index
    else 0

  member this.add_entry(s:string,t:lltype,a:expr option, frame:table_frame) = 
    if not(this.current_frame.entries.ContainsKey(s)) then
      this.global_index <- this.global_index + 1
      this.current_frame.entries.Add(s,LambdaDef(t, this.global_index, frame, a))
      this.global_index
    else 0

  member this.push_frame(n,line,column) =
    let newframe =
      {
        table_frame.name=n;
        entries=HashMap<string,TableEntry>();
        parent_scope = Some(this.current_frame);
      }
    this.frame_hash.[(line,column)] <- newframe
    this.current_frame = newframe

  member this.pop_frame() =
    this.current_frame.parent_scope |> map (fun p -> this.current_frame<-p)

  //ADD MORE MEMBERS
 
  member this.parent(f: table_frame option) =
    if f <> None then
      f |> get |> fun frame -> frame.parent_scope
    else f

  member this.find_frame(s:string, gi:int) = 
    let mutable frame = Some(this.current_frame)
    let mutable found = false
    while not(found) && frame <> None do
      //Check current frame
      found <- frame |> get |> fun f -> 
        (f.entries.ContainsKey(s) && this.match_gi(f.entries.[s],gi))
      if not(found) then
        frame <- this.parent(frame)
    frame

  member this.match_gi(t:TableEntry,gi:int) =
    if gi=0 then true
    else 
      match t with
        | SimpleDef(l,g,a) when g=gi -> true
        | LambdaDef(l,g,f,a) when g=gi -> true
        | _ -> false

  member this.get_entry(s:string, gi:int) = 
    let frame = this.find_frame(s,gi)
    if frame <> None then
      frame |> get |> fun f -> Some(f.entries.[s])
    else None 
  
  member this.get_entry_frame(s:string, gi:int) = 
    let frame = this.find_frame(s,gi)
    if frame <> None then
      frame |> get |> fun f -> (frame, Some(f.entries.[s]))
    else (None,None)

  member this.get_type(s:string, gi:int) = 
    let entry = this.get_entry(s,gi)
    if entry <> None then
      entry |> get |> fun e ->
        match e with
          | SimpleDef(l,g,a) -> l
          | LambdaDef(l,g,f,a) -> l
    else LLuntypable

  member this.set_type(s:string, gi:int, ltype:lltype) = 
    let (frame,entry) = this.get_entry_frame(s, gi)
    let mutable new_entry = None
    if entry <> None then
      entry |> get |> fun e -> 
        new_entry <- 
          match e with
            | SimpleDef(l,g,a) -> Some(SimpleDef(ltype,g,a))
            | LambdaDef(l,g,f,a) -> Some(LambdaDef(ltype,g,f,a))
      this.update_entry(s, frame, new_entry)
    else false
  
  member this.update_entry(s:string, frame:table_frame option, 
    entry:TableEntry option) = 
    if frame <> None && entry <> None then
      frame |> get |> fun f -> 
        entry |> get |> fun e ->
          f.entries.set_Item(s,e)
      true
    else false

  member this.infer_type (expression:LBox<expr>) = 
    //let (|Lbox|) (t:Stackitem<'AT>) =  Lbox(t.value);
    match expression with 
      | Lbox(Integer(_)) -> LLint
      | Lbox(Floatpt(_)) -> LLfloat
      | Lbox(Strlit(_)) -> LLstring
      | Lbox(Nil) -> LLunit /////////confirm?
      | Lbox(Binop("+",a,b)) | Lbox(Binop("-",a,b)) | Lbox(Binop("*",a,b)) 
      | Lbox(Binop("/",a,b)) | Lbox(Binop("%",a,b)) ->
        let (atype,btype) = (this.infer_type(a), this.infer_type(b))
        if atype=btype && (numerical atype) then atype
        else 
          printfn "(%d,%d): TYPE ERROR: Can't perform operation between types %A and %A"
            a.line a.column atype btype
          LLuntypable
      | Lbox(Binop(">",a,b)) | Lbox(Binop(">=",a,b)) | Lbox(Binop("eq",a,b)) 
      | Lbox(Binop("=",a,b)) | Lbox(Binop("neq",a,b)) | Lbox(Binop("^",a,b)) 
      | Lbox(Binop("<",a,b)) | Lbox(Binop("<=",a,b)) ->
        let (atype,btype) = (this.infer_type(a), this.infer_type(b))
        if atype=btype && (numerical atype) then LLint
        else
          printfn "(%d,%d): TYPE ERROR: Can't perform operation between types %A and %A. %s"
            a.line a.column atype btype "The comparison operator used expects equivalent numerical types."
          LLuntypable
      | Lbox(Binop("cons",a,b)) ->
        let (atype,btype) = (this.infer_type(a), this.infer_type(b))
        let mutable inner_type = LLint
        match btype with
          | LList(LLint) -> inner_type <- LLint
          | LList(LLfloat) -> inner_type <- LLfloat
          | LList(LLstring) -> inner_type <- LLstring
          | _ -> inner_type <- LLuntypable

        if inner_type = LLuntypable then
          printfn "(%d,%d): TYPE ERROR: Operator expects type LList of atomic types, recieved type %A"
            b.line b.column btype
          LLuntypable
        else
          if atype <> inner_type then
            printfn "(%d,%d): TYPE ERROR: Value passed expected to match list type %A but recieved type %A"
              a.line a.column inner_type atype
            LLuntypable
          else LList(inner_type) 
      | Lbox(Binop("and",a,b)) | Lbox(Binop("or",a,b)) ->
        let (atype,btype) = (this.infer_type(a), this.infer_type(b))
        if atype=btype && (atype=LLint) then atype
        else
          printfn "(%d,%d): TYPE ERROR: Can't perform operation between types %A and %A. %s"
            a.line a.column atype btype "The logical operator used expects LLint."
          LLuntypable
      | Lbox(Uniop("display",a)) ->
        let atype = this.infer_type(a)
        if grounded_type atype then LLunit
        else 
          printfn "(%d,%d): TYPE ERROR: Can't display type %A. %s"
            a.line a.column atype "The display function expects a grounded type."
          LLuntypable
      | Lbox(Uniop("car",l)) ->
        let ltype = this.infer_type(l)
        match ltype with
          | LList(LLint) -> LLint
          | LList(LLfloat) -> LLfloat
          | LList(LLstring) -> LLstring
          | _ -> printfn "(%d,%d): TYPE ERROR: Operator expects type LList, recieved type %A"
                   l.line l.column ltype
                 LLuntypable
      | Lbox(Uniop("cdr",l)) ->
        let ltype = this.infer_type(l)
        match ltype with
          | LList(LLint) as t -> t
          | LList(LLfloat) as t -> t
          | LList(LLstring) as t -> t
          | _ -> printfn "(%d,%d): TYPE ERROR: Operator expects type LList, recieved type %A"
                   l.line l.column ltype
                 LLuntypable
      | Lbox(Uniop("~",a)) | Lbox(Uniop("not",a)) ->
        let atype = this.infer_type(a)
        if atype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Operator expects type LLint, recieved type %A"
            a.line a.column atype
          LLuntypable
        else atype
      | Lbox(Var(var)) -> 
        let ltype = this.get_type(var,0)
        if ltype = LLuntypable then
          printfn "(%d,%d): TYPE ERROR: Type of variable %s cannot be inferred." 
            expression.line expression.column var
          ltype
        else ltype 
      
      | Lbox(Define(var,value)) ->
        let valtype = this.infer_type(value)
        if grounded_type(valtype) then
          ///////////add entry
          valtype
        else
          printfn "(%d,%d): TYPE ERROR: Type of expression %A cannot be inferred."
            value.line value.column value.value
          LLuntypable
      | Lbox(Lambda(l,e)) ->
        let mutable i = 1
        let mutable breakloop = false
        //Shouldn't need to infer type of a string...
        let mutable ltype = this.infer_type(l.[0])
        while i < l.Length && not(breakloop) do
          ltype <- this.infer_type(l.[i])
          if not(grounded_type(l.[i])) then ////////atomic type or grounded type?
            breakloop <- true
          i <- i + 1
        if breakloop then
          i <- i - 1
          printfn "(%d,%d): TYPE ERROR: All types passed to a lambda expression must be grounded, %A was passed"
            l.[i].line l.[i].column ltype
          LLuntypable
        else
          let etype = this.infer_type(e)
          if not(grounded_type(etype)) then
            printfn "(%d,%d): TYPE ERROR: Lambda expression must be a grounded type, type %A was passed"
              e.line e.column etype
            LLuntypable
          else etype

      | Lbox(Ifelse(a,b,c)) ->
        let (atype,btype,ctype) = 
          (this.infer_type(a), this.infer_type(b), this.infer_type(c))
        if atype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Ifelse expression expected LLint as first argument, %A was passed."
            a.line a.column atype
          LLuntypable
        else if btype <> ctype then
          printfn "(%d,%d): TYPE ERROR: Ifelse branch types must match, the first branch returned type %A, the second branch returned type %A."
            b.line b.column btype ctype
          LLuntypable
        else btype
      | Lbox(Whileloop(a,b)) ->
        let (atype, btype) = (this.infer_type(a), this.infer_type(b))
        if atype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Whileloop expression expected LLint as first argument, %A was passed."
            a.line a.column atype
          LLuntypable
        if not(grounded_type(btype)) then
          printfn "(%d,%d): TYPE ERROR: All expressions in whileloop must be grounded, type %A was passed."
            b.line b.column btype
        else btype

      /////Should sequence return type of final expression?
      | Lbox(Beginseq(se)) | Lbox(Sequence(se)) ->
        let mutable setype = LLint
        let mutable breakloop = false
        let mutable i = 0
        while not(breakloop) && i < se.Length-1 do
          setype <- this.infer_type(se.[i])
          if not(grounded_type(setype)) then
            breakloop <- true
          i <- i + 1
        if breakloop then 
          i <- i - 1
          printfn "(%d,%d): TYPE ERROR: Expected all types to be grounded in sequence, recieved type %A"
            se.[i].line se.[i].column setype
          LLuntypable
        else setype
      | Lbox(Setq(var,value)) ->
        let vartype = this.get_type(var.value,0)
        if not(grounded_type(vartype)) then
          printfn "(%d,%d): TYPE ERROR: Undeclared variable %s must be initialized before assignment"
            var.line var.column var.value
          LLuntypable
        else
          let valtype = this.infer_type(value)
          if vartype <> valtype then
            printfn "(%d,%d): TYPE ERROR: Value in assignment expected type %A but recieved type %A"
              value.line value.column vartype valtype
            LLuntypable
          else vartype 

// global symbol table
let mutable global_frame = // root frame
  {
    table_frame.name = "global";
    entries= HashMap<string,TableEntry>();
    parent_scope = None;
  }
let symbol_table =
  {
    SymbolTable.current_frame=global_frame;
    global_index = 0;
    frame_hash = HashMap<(int*int),table_frame>();
  }
