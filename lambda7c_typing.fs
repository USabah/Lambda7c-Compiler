module lambda7c
open System
open System.Collections.Generic
open lambda7c
open Recless.Base
open Option

//fsharpc lambda7c_typing.fs -r lambda7c_ast.dll

//lltype Methods
let rec grounded_type = function 
  | LLunknown | LLvar(_) | LLuntypable -> false
  | LLarray(t,_) -> grounded_type t
  | lltype.LLtuple(vs) -> List.forall grounded_type vs
  | LLclosure(args,rtype,_) -> List.forall grounded_type (rtype::args)
  | _ -> true

let numerical = function
  | LLint | LLfloat -> true
  | _ -> false 

let funtype = function
  | LLclosure(_) -> true
  | _ -> false

//Symbol Table

type TableEntry =
  | SimpleDef of typeof:lltype * gindex:int * ast_rep:option<expr>
  | LambdaDef of typeof:lltype * gindex:int * frame:table_frame * ast_rep:option<expr>
and table_frame =
  {
    name : string;
    entries:HashMap<string, TableEntry>;
    parent_scope:table_frame option;
    mutable closure:SortedDictionary<(string*int),lltype>;
  }

type SymbolTable =  // wrapping structure for symbol table frames
  {
     mutable current_frame: table_frame;
     mutable global_index: int;
     frame_hash: HashMap<(int*int),table_frame>;
     exported: HashMap<string, lltype>; // map of closure names to type definition
     // maps each closure to its table frame and a hashmap that maps each field in the closure struct 
     //to its position and type Hash(closure_name, (closure_frame, Hash(field, (pos, type))))
     clsmaps: HashMap<string,(table_frame*HashMap<string,(int*lltype)>)>;
     struct_ind: HashMap<string,int>;
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

  member this.overwrite_entry(s:string, t:lltype, a:expr option) =
    //check if entry exists, if not, add_entry will add it
    if this.current_frame.entries.ContainsKey(s) then
      //this.global_index <- this.global_index + 1
      let (_,gi) = this.get_entry_index(s) 
      this.current_frame.entries.[s] <- SimpleDef(t,gi,a)
      gi
    else 0 //entry doesn't exist
  
  member this.overwrite_entry(s:string, t:lltype, a:expr option, frame:table_frame) =
    //check if entry exists, if not, add_entry will add it
    if this.current_frame.entries.ContainsKey(s) then
      //this.global_index <- this.global_index + 1
      let (_,gi) = this.get_entry_index(s) 
      this.current_frame.entries.[s] <- LambdaDef(t,gi,frame,a)
      gi
    else 0

  member this.push_frame(n,line,column) =
    let newframe =
      {
        table_frame.name=n;
        entries=HashMap<string,TableEntry>();
        parent_scope = Some(this.current_frame);
        closure = SortedDictionary<(string*int),lltype>();
      }
    this.frame_hash.[(line,column)] <- newframe
    this.current_frame <- newframe

  member this.set_closure(closure: SortedDictionary<(string*int), lltype>, 
                          f: table_frame) = 
    f.closure <- closure

  member this.pop_frame() =
    this.current_frame.parent_scope |> map (fun p -> this.current_frame<-p)

  member this.parent(f: table_frame option) =
    if isSome f then
      f.Value |> fun frame -> frame.parent_scope
    else f

  member this.find_frame(s:string, gi:int) = 
    let mutable frame = Some(this.current_frame)
    let mutable found = false
    while not(found) && isSome frame do
      //Check current frame
      found <- frame.Value |> fun f -> 
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
    if isSome frame then
      frame.Value |> fun f -> Some(f.entries.[s])
    else None 
 
  member this.get_entry_index(s:string) = 
    let frame = this.find_frame(s,0)
    if isSome frame then
      frame.Value |> 
        fun f -> 
          match f.entries.[s] with
            | SimpleDef(_,g,_) -> (Some(f.entries.[s]), g)
            | LambdaDef(_,g,_,_) -> (Some(f.entries.[s]), g)
    else (None, 0)

  member this.get_index(s:string) =
    snd (this.get_entry_index(s))

  member this.get_entry_frame(s:string, gi:int) = 
    let frame = this.find_frame(s,gi)
    if isSome frame then
      frame.Value |> fun f -> (frame, Some(f.entries.[s]))
    else (None,None)

  member this.get_type(s:string, gi:int) = 
    let entry = this.get_entry(s,gi)
    if isSome entry then
      entry.Value |> fun e ->
        match e with
          | SimpleDef(l,g,a) -> l
          | LambdaDef(l,g,f,a) -> l
    else LLuntypable
  
  member this.set_index(s:string, index:int) = 
    let (frame,entry) = this.get_entry_frame(s, 0)
    let mutable new_entry = None
    if isSome entry && isSome frame then
      entry.Value |> fun e -> 
        new_entry <- 
          match e with
            | SimpleDef(l,g,a) -> Some(SimpleDef(l,index,a))
            | LambdaDef(l,g,f,a) -> Some(LambdaDef(l,index,f,a))
        this._update_entry(s, frame, new_entry)
    else false

  member this.set_type(s:string, gi:int, ltype:lltype) = 
    let (frame,entry) = this.get_entry_frame(s, gi)
    let mutable new_entry = None
    if isSome entry then
      entry.Value |> fun e -> 
        new_entry <- 
          match e with
            | SimpleDef(l,g,a) -> Some(SimpleDef(ltype,g,a))
            | LambdaDef(l,g,f,a) -> Some(LambdaDef(ltype,g,f,a))
      this._update_entry(s, frame, new_entry)
    else false
  
  member this._update_entry(s:string, frame:table_frame option, 
    entry:TableEntry option) = 
    if isSome frame && isSome entry then
      frame.Value |> fun f -> 
        entry.Value |> fun e ->
          f.entries.set_Item(s,e)
      true
    else false

  member this.find_closure() = 
    let fvs = SortedDictionary<(string*int),lltype>()
    this._collect_freevars(fvs)
    fvs
  
  member this._collect_freevars(fvs:SortedDictionary<(string*int),lltype>) = 
    let mutable parent_frame = this.parent(Some(this.current_frame)) 
    if isSome parent_frame then
      for entry in parent_frame.Value.entries do
        let (x,xentry) = (entry.Key, entry.Value)
        let mutable gindex = 0
        let t = 
          match xentry with
            | SimpleDef(lt,gi,_) -> gindex <- gi; lt 
            | LambdaDef(lt,gi,_,_) -> gindex <- gi; lt
        match t with
          | LLclosure(_) -> () //skip functions
          | _ -> fvs.Add((x,gindex), t); ()

  member this.find_full_closure(expression:LBox<expr>) =
    let fvs = SortedDictionary<(string*int),lltype>()
    this._collect_used_freevars(fvs,expression)
    fvs
    //logic
  (*  
    match cases in expression to see if Var(x) is a free variable
    It's a free variable if it hasn't been defined locally
    Maybe keep track of Defined variables in a set? 
  *)
  
  member this._collect_used_freevars(fvs:SortedDictionary<(string*int), lltype>, expression:LBox<expr>) = 
    match expression with
      | Lbox(Var(var)) | Lbox(TypedVar(_,var)) -> 
        if not(this.current_frame.entries.ContainsKey(var)) then
          //get gindex and lltype
          let entry_opt = this.get_entry(var,0)
          if isSome entry_opt then
            let (gindex, rt) = 
              match entry_opt.Value with
                | SimpleDef(lt,gi,_) -> (gi,lt)
                | LambdaDef(lt,gi,_,_) -> (gi,lt)
            if not(fvs.ContainsKey(var,gindex)) then
              fvs.Add((var,gindex), rt)
              ()
          
      | Lbox(Define(_,e1)) | Lbox(TypedDefine(_,e1)) | Lbox(Uniop(_,e1)) | Lbox(Lambda(_,e1)) | Lbox(TypedLambda(_,_,e1)) 
      | Lbox(Setq(_,e1)) ->
        this._collect_used_freevars(fvs, e1)
      | Lbox(Binop(_,e1,e2)) | Lbox(Whileloop(e1,e2)) | Lbox(Let(_,e1,e2)) | Lbox(TypedLet(_,e1,e2)) 
      | Lbox(VectorMake(e1,e2)) | Lbox(VectorGet(e1,e2)) ->
        this._collect_used_freevars(fvs, e1)
        this._collect_used_freevars(fvs, e2)
      | Lbox(Sequence(e1list)) | Lbox(Beginseq(e1list)) | Lbox(Vector(e1list)) ->
        for e1 in e1list do
          this._collect_used_freevars(fvs, e1)
      | Lbox(Ifelse(e1,e2,e3)) | Lbox(VectorSetq(e1,e2,e3)) ->
        this._collect_used_freevars(fvs, e1)
        this._collect_used_freevars(fvs, e2)
        this._collect_used_freevars(fvs, e3)
      | _ -> ()

  member this.get_current_closure() = 
    this.current_frame.closure

  member this.check_tlambda (identifier:string,expression:LBox<expr>) = 
    match expression with
      | Lbox(TypedLambda(l,_,e)) | Lbox(Lambda(l,e)) ->
        let mutable rt = LLunknown
        match expression.value with
          | TypedLambda(a,b,c) -> rt <- b
          | _ -> ()
        let exline = expression.line
        let excol = expression.column
        this.push_frame(identifier, exline, excol)
        let mutable index = 0
        let mutable error = false
        let mutable llvec = Vec<lltype>()
        let mutable iterator = 0
        for i in l do
          match i.value with
            //currently compiler does not support type inference for arguments
            | (None,v) -> 
              index <- this.add_entry(v, LLunknown, Some(e.value)) 
              llvec.Add(LLunknown)
              //If adding type inference for arguments, add unknown entry to  and update later
            | (Some(t),v) ->
              match t with
                | LLclosure(_,_,n) ->
                  let mutable closure_type = LLunknown
                  if this.exported.ContainsKey(n) then
                    closure_type <- this.exported.[n]
                  else
                    printfn "(%d,%d): TYPE ERROR: Argument %s is instantiated as a closure type %s, but that type has not been exported"
                      e.line e.column v n
                    error <- true
                  index <- this.add_entry(v, closure_type, Some(e.value), this.current_frame)
                  llvec.Add(closure_type)
                | _ -> 
                  index <- this.add_entry(v, t, Some(e.value)) 
                  llvec.Add(t)
          if index=0 then
            printfn "(%d,%d): TYPE ERROR: Multiple function arguments passed were assigned the variable name %s"
              e.line e.column (snd i.value)
            error <- true
          iterator <- iterator + 1
        
        if not(error) then
          let llist = Seq.toList llvec
          let mutable c_name = ""
          if not(this.struct_ind.ContainsKey(identifier)) then
            c_name <- sprintf "%s_%d" identifier this.global_index
            this.struct_ind.Add(identifier, this.global_index)
            this.global_index <- this.global_index + 1
          else
            c_name <- sprintf "%s_%d" identifier (this.struct_ind.[identifier])
          let expected_type = LLclosure(llist, rt, c_name)
          let gi = this.add_entry(identifier, expected_type, Some(expression.value), this.current_frame)
          if gi = 0 then
            printfn "(%d,%d): TYPE ERROR: Function arguments were assigned the same name as the function calling it %s"
              expression.line expression.column identifier
            0
          else 
            let etype = this.infer_type(e)
            if rt=etype || (rt=LLunknown && grounded_type(etype)) then
              let success = this.set_type(identifier, 0, LLclosure(llist, etype, c_name)) 
              if not(success) then 0
              else this.global_index 
            else
              printfn "(%d,%d): TYPE ERROR: Lambda expression must be a grounded type, type %A was passed"
                e.line e.column etype
              0
        else 0
      | _ -> 
        printfn "PROGRAMMER ERROR"
        0


  member this.infer_type (expression:LBox<expr>) = 
    if TRACE then
      let trace = 
        match expression with
          | Lbox(Define(_)) -> "Define"
          | Lbox(TypedDefine(_)) -> "TypedDefine"
          | Lbox(Var(_)) -> "Var"
          | Lbox(TypedVar(_)) -> "TypedVar"
          | Lbox(Lambda(_)) | Lbox(TypedLambda(_)) -> "Lambda/Typed"
          | Lbox(Let(_)) -> "Let"
          | Lbox(TypedLet(_)) -> "TypedLet"
          | Lbox(Sequence(_)) | Lbox(Beginseq(_)) -> sprintf "Sequence/Begin :: %A" expression
          | _ -> sprintf "OTHER: %A" expression
      printfn "%s" trace
    
    //let (|Lbox|) (t:Stackitem<'AT>) =  Lbox(t.value);
    match expression with 
      | Lbox(Integer(_)) -> LLint
      | Lbox(Floatpt(_)) -> LLfloat
      | Lbox(Strlit(_)) -> LLstring
      | Lbox(Nil) -> LLunit 
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
        let mutable inner_type = LLunknown
        let mutable size = 0
        match btype with
          | LLarray(LLint,s) -> inner_type <- LLint; size <- s
          | LLarray(LLfloat,s) -> inner_type <- LLfloat; size <- s
          | LLarray(LLstring,s) -> inner_type <- LLstring; size <- s
          | _ -> inner_type <- LLuntypable
        if inner_type = LLuntypable then
          printfn "(%d,%d): TYPE ERROR: Operator expects type LLarray of atomic types, recieved type %A"
            b.line b.column btype
          LLuntypable
        else
          if atype <> inner_type then
            printfn "(%d,%d): TYPE ERROR: Value passed expected to match list type %A but recieved type %A"
              a.line a.column inner_type atype
            LLuntypable
          else LLarray(inner_type,size) 
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
          | LLarray(LLint,_) -> LLint
          | LLarray(LLfloat,_) -> LLfloat
          | LLarray(LLstring,_) -> LLstring
          | _ -> printfn "(%d,%d): TYPE ERROR: Operator expects type LLarray, recieved type %A"
                   l.line l.column ltype
                 LLuntypable
      | Lbox(Uniop("cdr",l)) ->
        let ltype = this.infer_type(l)
        match ltype with
          | LLarray(LLint,_) as t -> t
          | LLarray(LLfloat,_) as t -> t
          | LLarray(LLstring,_) as t -> t
          | _ -> printfn "(%d,%d): TYPE ERROR: Operator expects type LLarray, recieved type %A"
                   l.line l.column ltype
                 LLuntypable
      | Lbox(Uniop("~",a)) ->
        let atype = this.infer_type(a)
        if atype <> LLint && atype <> LLfloat then
          printfn "(%d,%d): TYPE ERROR: Operator expects type LLint or LLfloat, recieved type %A"
            a.line a.column atype
          LLuntypable
        else atype
      | Lbox(Uniop("not",a)) ->
        let atype = this.infer_type(a)
        if atype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Operator expects type LLint, recieved type %A"
            a.line a.column atype
          LLuntypable
        else atype
      | Lbox(Var(var)) ->
        let ltype = this.get_type(var,0)
        if ltype = LLuntypable then
          printfn "(%d,%d): TYPE ERROR: Variable %s has not been defined." 
            expression.line expression.column var
          ltype
        else ltype 
      | Lbox(Define(Lbox(_,var),Lbox(Vector(l)))) 
      | Lbox(TypedDefine(Lbox(_,var),Lbox(Vector(l)))) ->
        let size = l.Length
        let (expected_type, expected_size) =
          match expression with
            | Lbox(Define(_)) -> (LLunknown,size)
            | Lbox(TypedDefine(Lbox(Some(LLarray(t,-1)),_),_)) -> (t,size)
            | Lbox(TypedDefine(Lbox(Some(LLarray(t,s)),_),_)) -> (t,s)
            | _ -> (LLuntypable, -1)
        let mutable etype = LLunknown
        if l.Length > 0 then
          etype <- this.infer_type(l.[0])
          if etype=LLuntypable then
            printfn "(%d,%d): TYPE ERROR: Could not infer type of expression for Vector"
              l.[0].line l.[0].column
            LLuntypable
          else if etype <> expected_type && expected_type <> LLunknown then
            printfn "(%d,%d): TYPE ERROR: Assigned type %A for Vector %s doesn't match the inferred type %A"
              l.[0].line l.[0].column expected_type var etype
            LLuntypable
          else if size <> expected_size then
            printfn "(%d,%d): TYPE ERROR: Assigned size %d for Vector %s doesn't equal the actual size of the vector, which is %d"
              l.[0].line l.[0].column expected_size var size
            LLuntypable
          else
            let mutable itype = LLunknown
            let mutable error = false
            for i in 1..size-1 do
              itype <- this.infer_type(l.[i])
              if itype <> etype then
                error <- true
            if error then 
              printfn "(%d,%d): TYPE ERROR: All values in a vector must be of the same type. Types %A and %A were passed"
                l.[0].line l.[0].column etype itype
              LLuntypable
            else 
              let vtype = LLarray(etype, size) 
              let i = this.add_entry(var, vtype, Some(expression.value))
              if i = 0 then
                let entry_type = this.get_type(var,0)
                if entry_type = vtype then
                  this.overwrite_entry(var, vtype, Some(expression.value)) |> ignore
                  vtype
                else
                  printfn "(%d,%d): TYPE ERROR: Cannot overwrite variable %s which has type %A with type %A"
                    expression.line expression.column var entry_type vtype
                  LLuntypable
              else vtype
        else
          printfn "(%d,%d): TYPE ERROR: Vector cannot be defined with size 0." 
            expression.line expression.column
          LLuntypable
      | Lbox(Define(Lbox(_,var),Lbox(VectorMake(e1,e2)))) 
      | Lbox(TypedDefine(Lbox(_,var),Lbox(VectorMake(e1,e2)))) ->
        let (e1type, e2type) = (this.infer_type(e1),this.infer_type(e2))
        let (expected_type, expected_size) =
          match expression with
            | Lbox(Define(_)) -> (LLunknown,-1)
            | Lbox(TypedDefine(Lbox(Some(LLarray(t,s)),_),_)) -> (t,s)
            | _ -> (LLuntypable, -1)
        if e2type <> LLint then
          printfn "(%d,%d): TYPE ERROR: The last argument of vmake should be its size (type LLint). Type %A was passed"
            e2.line e2.column e2type
          LLuntypable
        else if e1type<>LLstring && not(numerical e1type) then
          printfn "(%d,%d): TYPE ERROR: Vectors can only contain atomic types (int/string/float). Type %A was passed"
            e1.line e1.column e1type
          LLuntypable
        else
          let vtype = LLarray(e1type, expected_size) //MUST CHECK SIZE IN COMPILE TIME
          let i = this.add_entry(var, vtype, Some(expression.value))
          if i = 0 then
            let entry_type = this.get_type(var,0)
            if entry_type = vtype then
              this.overwrite_entry(var, vtype, Some(expression.value)) |> ignore
              vtype
            else
              printfn "(%d,%d): TYPE ERROR: Cannot overwrite variable %s which has type %A with type %A"
                expression.line expression.column var entry_type vtype
              LLuntypable
          else vtype
      | Lbox(Define(var,value)) ->
        let (_, identifier) = var.value
        match value.value with
          | TypedLambda(l,_,e) | Lambda(l,e) ->
            let rt = 
              match value.value with
                | TypedLambda(_,r,_) -> r
                | _ -> LLunknown
            let index = this.check_tlambda(identifier,value)
            //update closure
            //let fvs = this.find_closure() 
            let fvs = this.find_full_closure(e) 
            this.set_closure(fvs, this.current_frame)
            //printfn "TESTING: \nCLOSURE FOR variable %A: %A\n" 
              //identifier (this.current_frame.closure)
            this.pop_frame() |> ignore
            if index = 0 then
              LLuntypable
            else 
              let frame = this.frame_hash.[(value.line, value.column)]
              let entry = frame.entries.[identifier]
              let mutable inferred_type = LLunknown
              let mutable gi = 0
              match entry with
                | LambdaDef(l,g,f,a) -> 
                  inferred_type <- l
                  gi <- g
                | _ -> ()
              let i = this.add_entry(identifier, inferred_type, Some(expression.value), frame)
              //define should be able to overwrite previously defined entries
              let mutable error = false
              if i = 0 then //destructive assignment 
                let entry_type = this.get_type(identifier,0)
                if entry_type = inferred_type then
                  this.overwrite_entry(identifier, inferred_type, Some(expression.value), frame) |> ignore
                else
                  error <- true
                  printfn "(%d,%d): TYPE ERROR: Cannot overwrite variable %s which has type %A with type %A"
                    expression.line expression.column identifier entry_type inferred_type
              if error then
                LLuntypable
              else
                let old_entry = this.get_entry(identifier, 0)
                this.set_index(identifier, gi) |> ignore
                //printfn "(%d,%d) TEST: INFERRED TYPE %A FOR VARIABLE %s"
                  //expression.line expression.column (this.get_type(identifier, 0)) identifier
                this.get_type(identifier, 0) 
          | _ -> 
            let vtype = this.infer_type(value)
            if grounded_type(vtype) then
              let mutable i = 0
              let mutable isClosure = false
              match vtype with
                | LLclosure(_) ->
                  i <- this.add_entry(identifier, vtype, Some(expression.value), this.current_frame)
                  isClosure <- true
                | _ ->
                  i <- this.add_entry(identifier, vtype, Some(expression.value))
              let mutable error = false
              if i = 0 then //destructive assignment 
                let entry_type = this.get_type(identifier,0)
                if entry_type = vtype then
                  if isClosure then
                    this.overwrite_entry(identifier, vtype, Some(expression.value), this.current_frame) |> ignore
                  else
                    this.overwrite_entry(identifier, vtype, Some(expression.value)) |> ignore
                else
                  printfn "(%d,%d): TYPE ERROR: Cannot overwrite variable %s which has type %A with type %A"
                    expression.line expression.column identifier entry_type vtype 
                  error <- true
              if error then LLuntypable
              else
                //printfn "(%d,%d) TEST: INFERRED TYPE %A FOR VARIABLE %s"
                  //expression.line expression.column (this.get_type(identifier, 0)) identifier
                this.get_type(identifier, 0)
            else
              printfn "(%d,%d): TYPE ERROR: Type of expression for variable %s cannot be inferred"
                value.line value.column identifier
              LLuntypable
      | Lbox(TypedDefine(tvar, value)) ->
          let (t,identifier) = tvar.value
          let vtype = this.infer_type(value)
          if vtype=t.Value then
            let i = this.add_entry(identifier, vtype, Some(expression.value))
            let mutable error = false
            if i = 0 then //destructive assignment 
              let entry_type = this.get_type(identifier,0)
              if entry_type = vtype then
                this.overwrite_entry(identifier, vtype, Some(expression.value)) |> ignore
              else
                error <- true
                printfn "(%d,%d): TYPE ERROR: Cannot overwrite variable %s which has type %A with type %A"
                  expression.line expression.column identifier entry_type vtype
            if error then LLuntypable
            else this.get_type(identifier, 0)
          else 
            //case where value is [] or closure
            let inner_type = 
              match t.Value with
                | LLarray(x,_) | LLclosure(_,x,_) -> x 
                | _ -> LLuntypable
            match vtype with
              | LLarray(LLunknown,_) | LLclosure(_,LLunknown,_) ->
                let i = this.add_entry(identifier, t.Value, Some(expression.value), this.current_frame)
                let mutable error = false
                if i = 0 then //destructive assignment 
                  let entry_type = this.get_type(identifier,0)
                  if entry_type = t.Value then
                    this.overwrite_entry(identifier, t.Value, Some(expression.value), this.current_frame) |> ignore
                  else
                    error <- true
                    printfn "(%d,%d): TYPE ERROR: Cannot overwrite variable %s which has type %A with type %A"
                      expression.line expression.column identifier entry_type t.Value
                if error then LLuntypable
                else 
                  //printfn "TEST:::: VTYPE FOR VAR %s IS %A"
                    //identifier (this.get_type(identifier, 0))
                  this.get_type(identifier, 0)
              | _ ->
                printfn "(%d,%d): TYPE ERROR: Assigned type %A for variable %s does not match value type %A"
                  value.line value.column t.Value identifier vtype
                LLuntypable
      | Lbox(Let(var_tupl, value, e)) | Lbox(TypedLet(var_tupl, value, e)) -> 
        let (t, identifier) = var_tupl.value
        match value.value with
          | Lambda(l,e) | TypedLambda(l,_,e) ->
            LLuntypable
          | _ ->
            let vtype = this.infer_type(value)
            if not(grounded_type(vtype)) then
              printfn "(%d,%d): TYPE ERROR: Value assigned to variable %s cannot be inferred"
                value.line value.column identifier
              LLuntypable
            else
              let exline = expression.line
              let excol = expression.column
              //push frame and add value entry
              this.push_frame(identifier, exline, excol)
              let index =
                match t with
                  | None -> this.add_entry(identifier, vtype, Some(e.value))
                  | Some(ltype) -> 
                    if ltype <> vtype then
                      printfn "(%d,%d): TYPE ERROR: Variable %A has specified type %A which does not match inferred type %A"
                        value.line value.column identifier ltype vtype 
                      0
                    else
                      this.add_entry(identifier, vtype, Some(e.value))
              let etype = this.infer_type(e)
              //update closure
              //let fvs = this.find_closure() 
              let fvs = this.find_full_closure(e) 
              this.set_closure(fvs, this.current_frame)
              //printfn "TESTING: \nCLOSURE FOR variable %A: %A\n" 
                //identifier (this.current_frame.closure)
              this.pop_frame() |> ignore
              if index = 0 then
                LLuntypable
              else if not(grounded_type(etype)) then
                printfn "(%d,%d): TYPE ERROR: Let expression evaluated to type %A"
                  e.line e.column etype
                LLuntypable
              else etype
      | Lbox(Ifelse(a,b,c)) ->
        let (atype,btype,ctype) = 
          (this.infer_type(a), this.infer_type(b), this.infer_type(c))
        if atype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Ifelse expression expected an integer as first argument, type %A was passed."
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
        else if not(grounded_type(btype)) then
          printfn "(%d,%d): TYPE ERROR: All expressions in whileloop must be grounded, type %A was passed."
            b.line b.column btype
          LLuntypable
        else LLunit //since the loop may not run at all
      | Lbox(Sequence(Lbox(Var("getint"))::args)) when args.Length=0 ->
        LLint
      | Lbox(Sequence(Lbox(Var("free"))::args)) when args.Length=1 ->
        match args.[0] with
          | Lbox(Var(_)) ->
            let arg_type = this.infer_type(args.[0])
            match arg_type with
              | LLclosure(_) ->
                LLunit
              | _ -> 
                printfn "(%d,%d): TYPE ERROR: Cannot deallocate memory for non closure type %A" 
                  expression.line expression.column arg_type
                LLuntypable
          | _ ->
            printfn "(%d,%d): TYPE ERROR: Memory can only be deallocated for expressions in the form of a variable" 
              expression.line expression.column 
            LLuntypable
      | Lbox(Sequence((Lbox(Var(func_name)) as var_box)::args)) ->
        //Function Application or Return Value
        //check if LambdaDef or SimpleDef
        let entry_opt = this.get_entry(func_name,0)
        if isNone entry_opt then
          printfn "(%d,%d): TYPE ERROR: Variable %s has not yet been defined"
            var_box.line var_box.column func_name
          LLuntypable
        else 
          let entry = entry_opt.Value
          let (typeList, return_type, func_frame_opt, global_index) =
            match entry with
              | LambdaDef(t,g,f,_) ->
                match t with
                  | LLclosure(l,r,_) -> (l,r,Some(f),g)
                  | _ -> ([LLuntypable], LLuntypable, None, 0) //Should be unreachable...
              | SimpleDef(t,g,a) -> ([t], LLuntypable, None, 0)
          if global_index = 0 then
            typeList.[0]
          else
            //check length of args 
            if typeList.Length <> args.Length then
              if args.Length = 0 then //returning the closure
                match entry with
                  | LambdaDef(t,_,_,_) -> t
                  | _ -> LLuntypable
              else
                printfn "(%d,%d): TYPE ERROR: function %s expects %d argument(s) but recieved %d" 
                  expression.line expression.column func_name typeList.Length args.Length
                LLuntypable
            else
              //check that args match arg type
              let mutable index = 0 
              let mutable breakloop = false
              while index < typeList.Length && not(breakloop) do
                let arg_type = this.infer_type(args.[index])
                if arg_type <> typeList.[index] then
                  printfn "(%d,%d): TYPE ERROR: function %s expected arg type %A argument(s) but recieved %A at position %d" 
                    (args.[index].line) (args.[index].column) func_name (typeList.[index]) arg_type index
                  breakloop <- true
                index <- index + 1
              if breakloop then
                LLuntypable
              else
                return_type
      | Lbox(Beginseq(se)) | Lbox(Sequence(se)) ->
        let mutable setype = LLunknown
        let mutable breakloop = false
        let mutable i = 0
        while not(breakloop) && i < se.Length do
          setype <- this.infer_type(se.[i])
          if not(grounded_type(setype)) then
            breakloop <- true
          i <- i + 1
        if breakloop then 
          i <- i - 1
          printfn "(%d,%d): TYPE ERROR: Expected all types to be grounded in sequence, recieved type %A"
            se.[i].line se.[i].column setype
          LLuntypable
        else 
          //printfn "(%d,%d) TEST: INFERRED TYPE %A FOR Sequence"
            //expression.line expression.column setype 
          setype
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
          else 
            vartype 
      | Lbox(VectorSetq(v,i,e)) ->
        let (itype,vtype,etype) = (this.infer_type(i), this.infer_type(v), this.infer_type(e))
        if itype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Vectors are indexed by integers. Type %A was passed as an index"
            i.line i.column itype
          LLuntypable
        else 
          let rtype = 
            match vtype with
              | LLarray(atomic_type,_) -> atomic_type
              | _ -> LLuntypable
          if rtype = LLuntypable then
            printfn "(%d,%d): TYPE ERROR: Cannot infer type of vector. Type %A was passed as an index"
              v.line v.column vtype
            LLuntypable
          else
            if etype <> rtype then
              printfn "(%d,%d): TYPE ERROR: Type of assigned expression must be equal to the vector type. Type %A was passed"
                e.line e.column etype
              LLuntypable
            else LLunit
      (* /////may not be necessary
      | Lbox(VectorMake(e1,e2)) ->
        let (e1type, e2type) = (this.infer_type(e1),this.infer_type(e2))
        if e2type <> LLint then
          printfn "(%d,%d): TYPE ERROR: The last argument of vmake should be its size (type LLint). Type %A was passed"
            e2.line e2.column e2type
          LLuntypable
        else if e1type<>LLstring && not(numerical e1type) then
          printfn "(%d,%d): TYPE ERROR: Vectors can only contain atomic types (int/string/float). Type %A was passed"
            e1.line e1.column e1type
          LLuntypable
        else
          LLarray(e1type, _)
      *)
      
      | Lbox(VectorGet(v,i)) ->
        let (itype,vtype) = (this.infer_type(i), this.infer_type(v))
        if itype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Vectors are indexed by integers. Type %A was passed as an index"
            i.line i.column itype
          LLuntypable
        else 
          let rtype = 
            match vtype with
              | LLarray(atomic_type,_) -> atomic_type
              | _ -> LLuntypable
          if rtype = LLuntypable then
            printfn "(%d,%d): TYPE ERROR: Cannot infer type of vector. Type %A was passed as an index"
              v.line v.column vtype
            LLuntypable
          else rtype
      | Lbox(Export(s)) ->
        if this.exported.ContainsKey(s) then
          LLunit
        else 
          let closure_type = this.get_type(s,0)
          if closure_type = LLuntypable then
            printfn "(%d,%d): TYPE ERROR: Exported type %s is either undefined or out of scope"
              expression.line expression.column s
            LLuntypable
          else
            match closure_type with
              | LLclosure(_) -> 
                this.exported.Add(s, closure_type) 
                LLunit
              | _ ->
                printfn "(%d,%d): TYPE ERROR: Exported type %s is not a closure type"
                  expression.line expression.column s
                LLuntypable
      | _ -> 
        printfn "(%d,%d): TYPE ERROR: %A Expression is not currently supported"
          expression.line expression.column expression.value 
        LLuntypable

// global symbol table
let mutable global_frame = // root frame
  {
    table_frame.name = "global";
    entries = HashMap<string,TableEntry>();
    parent_scope = None;
    closure = SortedDictionary<(string*int),lltype>(); 
  }
let symbol_table =
  {
    SymbolTable.current_frame=global_frame;
    global_index = 0;
    frame_hash = HashMap<(int*int),table_frame>();
    exported = HashMap<string, lltype>(); 
    clsmaps = HashMap<string,(table_frame*HashMap<string,(int*lltype)>)>();
    struct_ind = HashMap<string,int>(); 
  }

let makeSymbolTable = 
  let mutable g_frame = // root frame
    {
      table_frame.name = "global";
      entries = HashMap<string,TableEntry>();
      parent_scope = None;
      closure = SortedDictionary<(string*int),lltype>(); 
    }
  let symb_table =
    {
      SymbolTable.current_frame=g_frame;
      global_index = 0;
      frame_hash = HashMap<(int*int),table_frame>();
      exported = HashMap<string, lltype>(); 
      clsmaps = HashMap<string,(table_frame*HashMap<string,(int*lltype)>)>();
      struct_ind = HashMap<string,int>(); 
    }
  symb_table
