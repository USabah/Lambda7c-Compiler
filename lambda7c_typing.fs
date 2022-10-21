module lambda7c
open System
open System.Collections.Generic
open lambda7c
open Recless.Base
open Option

//fsharpc lambda7c_typing.fs -r lambda7c_ast.dll

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
  member this.add_entry(s:string,t:lltype,a:expr option) = //overwrite
    this.global_index <- this.global_index + 1
    this.current_frame.entries.Add(s,SimpleDef(t, this.global_index, a))
  
  member this.add_entry(s:string,t:lltype,a:expr option, frame:table_frame) = //overwrite
    this.global_index <- this.global_index + 1
    this.current_frame.entries.Add(s,LambdaDef(t, this.global_index, frame, a))

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
 
  member this.parent_frame(f: table_frame option) =
    if f <> None then
      f |> get |> fun frame -> frame.parent_scope
    else f

  member this.get_entry(s:string) = 
    let mutable frame = Some(this.current_frame)
    let mutable found = false
    while not(found) && frame <> None do
      //Check current frame
      found <- frame |> get |> fun f -> f.entries.ContainsKey(s)
      //Next frame
      if not(found) then
        frame <- this.parent_frame(frame)
        //frame <- frame |> get |> fun f -> f.parent_scope
    frame

  member this.infer_type (expression:expr) = 
    match expression with 
      | Integer(_) -> LLint
      | Binop("+",a,b) | Binop("-",a,b) -> 
        let (atype,btype) = (this.infer_type(a), this.infer_type(b))
        if atype=btype && (atype=LLint || atype=LLfloat) then atype
        else LLuntypable
      | Var(x) -> 
        let frame = this.get_entry(x)
        if frame <> None then
        else 
          printfn "Error: Undeclared Variable %s" x
          LLuntypable

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
