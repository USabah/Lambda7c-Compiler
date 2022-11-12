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
  | LambdaDef of typeof:lltype * gindex:int * frame:table_frame * ast_rep:option<expr>
and table_frame =
  {
    name : string;
    entries:HashMap<string, TableEntry>;
    parent_scope:table_frame option;
    mutable closure:SortedDictionary<string,(int*lltype)>;
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

  member this.overwrite_entry(s:string, t:lltype, a:expr option) =
    //check if entry exists, if not, add_entry will add it
    let success = this.add_entry(s,t,a) 
    if success = 0 then
      //potential to skip entries since overwriting old gi
      /////should we use the old global index?
      this.global_index <- this.global_index + 1
      this.current_frame.entries.[s] <- SimpleDef(t,this.global_index,a)
      this.global_index
    else this.global_index
  
  member this.overwrite_entry(s:string, t:lltype, a:expr option, frame:table_frame) =
    //check if entry exists, if not, add_entry will add it
    let success = this.add_entry(s,t,a) 
    if success = 0 then
      //potential to skip entries since overwriting old gi
      /////should we use the old global index?
      this.global_index <- this.global_index + 1
      this.current_frame.entries.[s] <- LambdaDef(t,this.global_index,frame,a)
      this.global_index
    else this.global_index

  member this.push_frame(n,line,column) =
    let newframe =
      {
        table_frame.name=n;
        entries=HashMap<string,TableEntry>();
        parent_scope = Some(this.current_frame);
        closure = SortedDictionary<string,(int*lltype)>();
      }
    this.frame_hash.[(line,column)] <- newframe
    this.current_frame <- newframe

  member this.set_closure(closure: SortedDictionary<string, (int*lltype)>, 
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
    let fvs = SortedDictionary<string,(int*lltype)>()
    this._collect_freevars(fvs)
    fvs
  
  member this._collect_freevars(fvs:SortedDictionary<string,(int*lltype)>) = 
    let mutable current_frame = this.parent(Some(this.current_frame)) 
    while isSome current_frame do
      for entry in current_frame.Value.entries do
        let (x,xentry) = (entry.Key, entry.Value)
        let mutable gindex = 0
        let t = 
          match xentry with
            | SimpleDef(lt,gi,_) -> gindex <- gi; lt 
            | LambdaDef(lt,gi,_,_) -> gindex <- gi; lt
        match t with
          | LLfun(_,_) -> () //skip functions
          | _ -> fvs.Add(x, (gindex, t)); ()
      current_frame <- this.parent(current_frame)

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
        for i in l do
          match i.value with
            | (None,v) -> 
              index <- this.add_entry(v, LLunknown, Some(e.value)) 
              llvec.Add(LLunknown)
            | (Some(t),v) -> 
              index <- this.add_entry(v, t, Some(e.value)) 
              llvec.Add(t)
          if index=0 then
            printfn "(%d,%d): TYPE ERROR: Multiple function arguments passed were assigned the variable name %s"
              e.line e.column (snd i.value)
            error <- true
        if not(error) then
          let llist = Seq.toList llvec
          let expected_type = LLfun(llist, rt)
          let gi = this.add_entry(identifier, expected_type, Some(expression.value), this.current_frame)
          if gi = 0 then
            printfn "(%d,%d): TYPE ERROR: Function arguments were assigned the same name as the function calling it %s"
              expression.line expression.column identifier
            0
          else 
            let etype = this.infer_type(e)
            if rt=etype || (rt=LLunknown && grounded_type(etype)) then
              let success = this.set_type(identifier, 0, LLfun(llist, etype)) 
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
    //let (|Lbox|) (t:Stackitem<'AT>) =  Lbox(t.value);
    match expression with 
      | Lbox(Integer(_)) -> LLint
      | Lbox(Floatpt(_)) -> LLfloat
      | Lbox(Strlit(_)) -> 
        LLstring
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
        let mutable inner_type = LLunknown
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
          printfn "(%d,%d): TYPE ERROR: Variable %s has not been defined." 
            expression.line expression.column var
          ltype
        else ltype 
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
            let fvs = this.find_closure() 
            this.set_closure(fvs, this.current_frame)
            printfn "TESTING: \nCLOSURE FOR variable %A: %A\n" 
              identifier (this.current_frame.closure)
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
              if i = 0 then
                this.overwrite_entry(identifier, inferred_type, Some(expression.value), frame) |> ignore
              let old_entry = this.get_entry(identifier, 0)
              //Do we need to update global index? like this.gi -= 1
              this.set_index(identifier, gi) |> ignore
              //printfn "(%d,%d) TEST: INFERRED TYPE %A FOR VARIABLE %s"
                //expression.line expression.column (this.get_type(identifier, 0)) identifier
              this.get_type(identifier, 0) 
          | _ -> 
            let vtype = this.infer_type(value)
            if grounded_type(vtype) then
              let i = this.add_entry(identifier, vtype, Some(expression.value))
              if i = 0 then
                this.overwrite_entry(identifier, vtype, Some(expression.value)) |> ignore
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
            if i = 0 then
              this.overwrite_entry(identifier, vtype, Some(expression.value)) |> ignore
            this.get_type(identifier, 0)
          else 
            //case where value is []
            let inner_type = 
              match t.Value with
                | LList(x) -> x
                | _ -> LLuntypable
            if inner_type <> LLuntypable then
              if vtype = LList(LLunknown) then
                let i = this.add_entry(identifier, t.Value, Some(expression.value))
                if i = 0 then
                  this.overwrite_entry(identifier, t.Value, Some(expression.value)) |> ignore
                this.get_type(identifier, 0)
              else
                printfn "(%d,%d): TYPE ERROR: Assigned type %A for variable %s does not match value type %A"
                  value.line value.column vtype identifier (t.Value)
                LLuntypable
            else
              printfn "(%d,%d): TYPE ERROR: Assigned type %A for variable %s does not match value type %A"
                value.line value.column vtype identifier (t.Value) 
              LLuntypable

      | Lbox(Let(var_tupl, value, e)) | Lbox(TypedLet(var_tupl, value, e)) -> 
        let (t, identifier) = var_tupl.value
        /////should lambda be allowed for let expressions
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
              let fvs = this.find_closure() 
              this.set_closure(fvs, this.current_frame)
              printfn "TESTING: \nCLOSURE FOR variable %A: %A\n" 
                identifier (this.current_frame.closure)
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
      | Lbox(Setq(var,value)) -> /////should this return LLunit?
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
      | Lbox(Vector(vl)) ->
        let mutable etype = LLunknown
        if vl.Length > 0 then
          etype <- this.infer_type(vl.[0])
          if etype=LLuntypable then
            printfn "(%d,%d): TYPE ERROR: Could not infer type of expression for Vector"
              vl.[0].line vl.[0].column
            LLuntypable
          else
            let mutable itype = LLunknown
            let mutable error = false
            for i in 1..vl.Length-1 do
              itype <- this.infer_type(vl.[i])
              if itype <> etype then
                error <- true
            if error then 
              printfn "(%d,%d): TYPE ERROR: All values in a vector must be of the same type. Types %A and %A were passed"
                vl.[0].line vl.[0].column etype itype
              LLuntypable
            else LList(etype) ///////originally etype?
        else LList(etype)
      | Lbox(VectorSetq(v,i,e)) ->
        let (itype,vtype,etype) = (this.infer_type(i), this.infer_type(v), this.infer_type(e))
        if itype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Vectors are indexed by integers. Type %A was passed as an index"
            i.line i.column itype
          LLuntypable
        else 
          let rtype = 
            match vtype with
              | LList(atomic_type) -> atomic_type
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
      | Lbox(VectorMake(e1,e2)) ->
        let (e1type, e2type) = (this.infer_type(e1),this.infer_type(e2))
        if e1type <> e2type then
          printfn "(%d,%d): TYPE ERROR: vmake expects two arguments of the same type. Types %A and %A were passed"
            expression.line expression.column e1type e2type
          LLuntypable
        else if e1type<>LLstring && not(numerical e1type) then
          printfn "(%d,%d): TYPE ERROR: Vectors can only contain atomic types (int/string/float). Type %A was passed"
            expression.line expression.column e1type
          LLuntypable
        else
          LList(e1type)
      | Lbox(VectorGet(v,i)) ->
        let (itype,vtype) = (this.infer_type(i), this.infer_type(v))
        if itype <> LLint then
          printfn "(%d,%d): TYPE ERROR: Vectors are indexed by integers. Type %A was passed as an index"
            i.line i.column itype
          LLuntypable
        else 
          let rtype = 
            match vtype with
              | LList(atomic_type) -> atomic_type
              | _ -> LLuntypable
          if rtype = LLuntypable then
            printfn "(%d,%d): TYPE ERROR: Cannot infer type of vector. Type %A was passed as an index"
              v.line v.column vtype
            LLuntypable
          else rtype
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
    closure = SortedDictionary<string, (int*lltype)>(); 
  }
let symbol_table =
  {
    SymbolTable.current_frame=global_frame;
    global_index = 0;
    frame_hash = HashMap<(int*int),table_frame>();
  }

let makeSymbolTable = 
  let mutable g_frame = // root frame
    {
      table_frame.name = "global";
      entries = HashMap<string,TableEntry>();
      parent_scope = None;
      closure = SortedDictionary<string, (int*lltype)>(); 
    }
  let symb_table =
    {
      SymbolTable.current_frame=g_frame;
      global_index = 0;
      frame_hash = HashMap<(int*int),table_frame>();
    }
  symb_table
