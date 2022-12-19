//LL(1) Grammar for lambda7c AST
module lambda7c
open System
open Recless.Base

////fsharpc lambda7c_ast.fs -a -r recless.dll -r schemer_lex.dll

///// AST Definition
type expr =
  | Integer of int
  | Floatpt of float
  | Strlit of string
  | Var of string
  | TypedVar of lltype option*string
  | Nil
  | Binop of string*LBox<expr>*LBox<expr>
  | Uniop of string*LBox<expr>
  | Ifelse of LBox<expr>*LBox<expr>*LBox<expr>
  | Whileloop of LBox<expr>*LBox<expr>
  | Define of LBox<lltype option*string>*LBox<expr>
  | TypedDefine of LBox<lltype option*string>*LBox<expr>
  | Lambda of (LBox<lltype option*string> list)*LBox<expr>
  | TypedLambda of (LBox<lltype option*string> list)*lltype*LBox<expr>
  | Let of LBox<lltype option*string>*LBox<expr>*LBox<expr>
  | TypedLet of LBox<lltype option*string>*LBox<expr>*LBox<expr>
  | Quote of expr               // may not need for now
  | Setq of LBox<string>*LBox<expr>   // destructive assignment
  | Sequence of LBox<expr> list  // includes function application (f x y z)
  // may want to change above for a more standard case for function application:
  // | Apply of expr*(expr list)
  | Beginseq of LBox<expr> list  // sequence of expressions
  //Vector expressions
  | Vector of LBox<expr> list  //[1 2 3]
  | VectorMake of LBox<expr>*LBox<expr> //(vmake 0 4)
  | VectorSetq of LBox<expr>*LBox<expr>*LBox<expr> //(vsetq  a 0 8)
  | VectorGet of LBox<expr>*LBox<expr>  //(vget a 1)
  // type expressions  
  | TypeExpr of lltype
  | Typedval of (lltype*expr)
  | Label of string   // not a proper expression - just a temporary
  | StrList of LBox<lltype option*string> list //Intermediate Addition
  | Export of string
  | Sizeopt of expr option
  | Error
  //  | Continuation of (expr -> expr)    // don't need

and lltype =  // abstract syntax for type expressions
  | LLint | LLfloat | LLstring
  | LList of lltype | LLtuple of lltype list
  | LLarray of lltype*int | LLfun of (lltype list)*lltype
  | LLclosure of (lltype list)*lltype*string
  | LLunknown | LLuntypable | LLvar of string | LLunit


let mutable Gmr = new_grammar<expr>("Axprplus") 

let binops = ["+";"-";"*";"/";"%";"^";"=";"<";">";"<=";">=";"cons";"neq";"eq";"and";"or"]
let uniops = ["~";"car";"cdr";"not";"display"]
let keywords = ["export";"NIL";"int";"unit";"float";"string";"unit";"let";"define";"begin";"lambda";"while";"setq";"if"] 
let vecops = ["vmake";"vsetq";"vget"]
let terminals = List.append (List.append (List.append keywords binops) uniops) vecops
Gmr.terminals(terminals)
// valueterminals must name a token type (Num) and a function to convert
// the token text to a value of the ast type.
Gmr.valueterminal("INT","Num",fun n -> Integer(int n))
Gmr.valueterminal("FLOAT","Float",fun f -> Floatpt(float f))
Gmr.valueterminal("STRLIT","StrLit",fun s -> Strlit(s.Trim('"')))
Gmr.valueterminal("VAR","Alphanum",fun s -> Var(string s))
Gmr.lexterminal("COLON",":") // map terminal names to token forms
Gmr.lexterminal("LPAREN","(") 
Gmr.lexterminal("RPAREN",")") 
Gmr.lexterminal("LBRACE","[") 
Gmr.lexterminal("RBRACE","]") 
Gmr.lexterminal("LBRACK","<") 
Gmr.lexterminal("RBRACK",">") 
Gmr.lexterminal("LBRACKEQ","<=") 
Gmr.lexterminal("RBRACKEQ",">=") 
//Gmr.nonterminals(["Expr";"Seq";"Binop";"Uniop";"Axpr";"Axprplus";"Typeopt";"Txpr";"VarTypeopt";"Strlist";"Sval";"Seq"]);  // these are in addition to AxprList
Gmr.nonterminals(["Expr";"Seq";"Binop";"Uniop";"Axpr";"AxprList";"Typeopt";"Txpr";"Sizeopt";"VarTypeopt";"Strlist";"Sval";"Seq"]);  // these are in addition to AxprList

let semact1 (rhs:Vec<Stackitem<expr>>) =  
  match (rhs.[0].value, rhs.[1].value) with
    | (a, Nil) -> let box = rhs.[0].tolbox(a) in Sequence(box::[])
    | (a, Sequence(b)) -> let box = rhs.[0].tolbox(a) in Sequence(box::b)
    | _ -> Error 
Gmr.production("AxprList --> Axpr AxprList", semact1)

Gmr.production("AxprList --> ", fun r -> Nil)
  
Gmr.production("Axpr --> LPAREN Expr RPAREN", fun r -> r.[1].value)

Gmr.production("Axpr --> INT", fun r -> r.[0].value)
Gmr.production("Axpr --> FLOAT", fun r -> r.[0].value)
Gmr.production("Axpr --> STRLIT", fun r -> r.[0].value)
Gmr.production("Axpr --> VAR", fun r -> r.[0].value)
Gmr.production("Axpr --> NIL", fun r -> Nil)

Gmr.production("Axprplus --> Axpr AxprList", fun rhs -> 
  match (rhs.[0].value, rhs.[1].value) with
    | (a,Nil) -> let vbox = rhs.[0] in Sequence(vbox::[])
    | (a,Sequence(l)) -> let vbox = rhs.[0] in Sequence(vbox::l)
    | _ -> Error
)

//Vector Ops
Gmr.production("Axpr --> LBRACE AxprList RBRACE", fun rhs -> 
  match (rhs.[1].value) with
    | Sequence(l) -> Vector(l)
    | Nil -> Vector([])
    | _ -> Error
)

Gmr.production("Expr --> vget Axpr Axpr", fun rhs -> 
  VectorGet(rhs.[1], rhs.[2])
)
Gmr.production("Expr --> vsetq Axpr Axpr Axpr", fun rhs -> 
  VectorSetq(rhs.[1], rhs.[2], rhs.[3])
)
Gmr.production("Expr --> vmake Axpr Axpr", fun rhs -> 
  VectorMake(rhs.[1], rhs.[2])
)

//export
Gmr.production("Expr --> export VAR", fun rhs -> 
  match rhs.[1].value with
    | Var(s) -> Export(s)
    | _ -> Error
)

//#Uniop and Binop

let semact2 (rhs:Vec<Stackitem<expr>>) =  
  let strval = 
    match (rhs.[0].value) with
      | Label(a) -> a
      | _ -> ""
  if strval = "" then Error
  else   
    let box1 = rhs.[1].tolbox(rhs.[1].value)
    let box2 = rhs.[2].tolbox(rhs.[2].value)
    Binop(strval, box1, box2)
Gmr.production("Expr --> Binop Axpr Axpr", semact2)

let semact3 (rhs:Vec<Stackitem<expr>>) =  
  let strval = 
    match (rhs.[0].value) with
      | Label(a) -> a
      | _ -> ""
  if strval = "" then Error
  else  
    let box1 = rhs.[1].tolbox(rhs.[1].value)
    Uniop(strval, box1)

Gmr.production("Expr --> Uniop Axpr", semact3)

Gmr.production("Binop --> +", fun r -> Label("+"))
Gmr.production("Binop --> -", fun r -> Label("-"))
Gmr.production("Binop --> *", fun r -> Label("*"))
Gmr.production("Binop --> /", fun r -> Label("/"))
Gmr.production("Binop --> %", fun r -> Label("%"))
Gmr.production("Binop --> ^", fun r -> Label("^"))
Gmr.production("Binop --> =", fun r -> Label("="))
Gmr.production("Binop --> LBRACK", fun r -> Label("<"))
Gmr.production("Binop --> RBRACK", fun r -> Label(">"))
Gmr.production("Binop --> LBRACKEQ", fun r -> Label("<="))
Gmr.production("Binop --> RBRACKEQ", fun r -> Label(">="))
Gmr.production("Binop --> cons", fun r -> Label("cons"))
Gmr.production("Binop --> neq", fun r -> Label("neq"))
Gmr.production("Binop --> eq", fun r -> Label("eq"))
Gmr.production("Binop --> and", fun r -> Label("and"))
Gmr.production("Binop --> or", fun r -> Label("or"))

Gmr.production("Uniop --> ~", fun r -> Label("~"))
Gmr.production("Uniop --> car", fun r -> Label("car"))
Gmr.production("Uniop --> cdr", fun r -> Label("cdr"))
Gmr.production("Uniop --> not", fun r -> Label("not"))
Gmr.production("Uniop --> display", fun r -> Label("display"))

//Ifelse, Whileloop, Define, Lambda 

Gmr.production(
  "Expr --> if Axpr Axpr Axpr ", 
  fun r -> Ifelse(r.[1].tolbox(r.[1].value), 
             r.[2].tolbox(r.[2].value), r.[3].tolbox(r.[3].value)))
Gmr.production("Expr --> while Axpr Axpr",
  fun r -> Whileloop(r.[1].tolbox(r.[1].value), 
             r.[2].tolbox(r.[2].value)))

//Types
Gmr.production("Txpr --> int", fun r -> TypeExpr(LLint))
Gmr.production("Txpr --> unit", fun r -> TypeExpr(LLunit))
Gmr.production("Txpr --> float", fun r -> TypeExpr(LLfloat))
Gmr.production("Txpr --> string", fun r -> TypeExpr(LLstring))

//Txpr --> LBRACE Txpr Sizeopt RBRACE
Gmr.production("Txpr --> LBRACE Txpr Sizeopt RBRACE", fun rhs -> 
  match (rhs.[1].value, rhs.[2].value) with
    | (TypeExpr(t),Sizeopt(Some(Integer(size)))) -> 
      (*if size <= 0 then
        printfn "PARSING ERROR line %d column %d, %s"
          (rhs.[2].line) (rhs.[2].column) "Vector cannot be instantiated with size <= 0."
        Error
      else*)
      TypeExpr(LLarray(t,size))
    | (TypeExpr(t),Sizeopt(None)) -> 
      TypeExpr(LLarray(t,-1))
    | _ -> Error
)

Gmr.production("Sizeopt --> INT", fun rhs -> Sizeopt(Some(rhs.[0].value)))
Gmr.production("Sizeopt --> ", fun rhs -> Sizeopt(None))

Gmr.production("Txpr --> LBRACK VAR RBRACK", fun rhs ->
  match rhs.[1].value with
    | Var(s) -> TypeExpr(LLclosure([],LLunknown,s))
    | _ -> Error
)
Gmr.production("Typeopt --> COLON Txpr", fun r -> r.[1].value)
Gmr.production("Typeopt --> ", fun r -> TypeExpr(LLunknown))


let semact4 (rhs:Vec<Stackitem<expr>>) =  
    match (rhs.[0].value, rhs.[1].value) with
      | (Var(a), TypeExpr(LLunknown)) -> Var(a)
      | (Var(a), TypeExpr(x)) -> TypedVar(Some(x),a)
      | _ -> Error
Gmr.production("VarTypeopt --> VAR Typeopt", semact4)

let semact5 (rhs:Vec<Stackitem<expr>>) =  
    match (rhs.[1].value) with
      | Var(s) -> 
          let varbox  = rhs.[1].tolbox((None,s)) 
          let exprbox = rhs.[2].tolbox(rhs.[2].value)
          Define(varbox, exprbox)
      | TypedVar(a,s) -> 
          let tvarbox = rhs.[1].tolbox(a,s) 
          let exprbox = rhs.[2].tolbox(rhs.[2].value)
          TypedDefine(tvarbox, exprbox)
      | _ -> Error
Gmr.production("Expr --> define VarTypeopt Axpr", semact5)

let semact6 (rhs:Vec<Stackitem<expr>>) =  
  match (rhs.[0].value, rhs.[1].value) with
    | (Var(a), Nil) -> 
      let varbox = rhs.[0].tolbox((None,a)) 
      StrList(varbox::[])
    | (TypedVar(a,b), Nil) -> 
      let varbox = rhs.[0].tolbox((a,b)) 
      StrList(varbox::[])
    | (Var(a), StrList(l)) -> 
      let varbox = rhs.[0].tolbox((None,a)) 
      StrList(varbox::l)
    | (TypedVar(a,b), StrList(l)) -> 
      let varbox = rhs.[0].tolbox((a,b)) 
      StrList(varbox::l)
    | _ -> Error
Gmr.production("Sval --> ", fun r -> Nil)
Gmr.production("Sval --> VarTypeopt Sval", semact6)
Gmr.production("Strlist --> VarTypeopt Sval", semact6)


//#Let, Setq

let semact7 (rhs:Vec<Stackitem<expr>>) =  
    match (rhs.[2].value) with
      | Var(s) -> 
          let varbox = rhs.[2].tolbox((None,s)) 
          let expr1box = rhs.[3].tolbox(rhs.[3].value)
          let expr2box = rhs.[5].tolbox(rhs.[5].value)
          Let(varbox, expr1box, expr2box)
      | TypedVar(a,s) -> 
          let tvarbox = rhs.[2].tolbox(a,s) 
          let expr1box = rhs.[3].tolbox(rhs.[3].value)
          let expr2box = rhs.[5].tolbox(rhs.[5].value)
          TypedLet(tvarbox, expr1box, expr2box)
      | _ -> Error
Gmr.production("Expr --> let LPAREN VarTypeopt Axpr RPAREN Axpr", semact7)


let semact8 (rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[1].value) with
    | Var(s) -> 
      let varbox = rhs.[1].tolbox(s) 
      let exprbox = rhs.[2].tolbox(rhs.[2].value)
      Setq(varbox, exprbox)
    | _ -> Error
Gmr.production("Expr --> setq VAR Axpr ", semact8)


//#Sequences

let semact9 (rhs:Vec<Stackitem<expr>>) =  
  match (rhs.[1].value, rhs.[2].value) with
    | (a, Nil) -> let box = rhs.[1].tolbox(a) in Beginseq(box::[])
    | (a, Sequence(b)) -> let box = rhs.[1].tolbox(a) in Beginseq(box::b)
    | _ -> Error
Gmr.production("Expr --> begin Axpr Seq", semact9)

Gmr.production("Seq --> Axpr Seq", semact1)
Gmr.production("Seq --> ", fun r -> Nil)

Gmr.production("Expr --> Seq", fun r -> r.[0].value)


let semact10 (rhs:Vec<Stackitem<expr>>) =  
  match (rhs.[2].value, rhs.[4].value) with
    | (StrList(a), TypeExpr(LLunknown)) -> 
      let exprbox = rhs.[5].tolbox(rhs.[5].value)
      Lambda(a, exprbox)
    | (StrList(a), TypeExpr(x))-> 
      let exprbox = rhs.[5].tolbox(rhs.[5].value)
      TypedLambda(a, x, exprbox)
    | _ -> Error
Gmr.production("Expr --> lambda LPAREN Strlist RPAREN Typeopt Axpr", semact10)


let mutable GENERATE = false
let argv = Environment.GetCommandLineArgs();
let mutable runfile = ""

if argv.Length > 1 then
  for i in 1..argv.Length-1 do
    match argv.[i] with
      | "-generate" -> GENERATE <- true
      | "-trace" -> TRACE <- true
      | a -> runfile <- a

// declare parser to be generated or loaded, init with default (null)
let mutable parser1:LLparser<expr> = Unchecked.defaultof<LLparser<expr>>

if GENERATE then
  parser1 <- make_parser(Gmr,null)  // GENERATES PARSER (with null lexer)
  parser1.to_json("lambda7c_ast")     // creates llcalc_ast.json
  if TRACE then (Gmr.printgrammar(); printfn "parser saved")
elif not(GENERATE) then
  parser1 <- load_parser_into(Gmr,"lambda7c_ast.json",null,false)
  if TRACE then (parser1.Gmr.printgrammar(); printfn "parser loaded")
