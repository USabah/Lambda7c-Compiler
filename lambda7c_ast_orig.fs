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
  | TypedVar of lltype*string
  | Nil
  | Binop of string*expr*expr
  | Uniop of string*expr
  | Ifelse of expr*expr*expr
  | Whileloop of expr*expr
  | Define of LBox<string>*expr
  | TypedDefine of LBox<lltype*string>*expr
  | Lambda of (string list)*expr
  | TypedLambda of ((lltype*string) list)*lltype*expr
  | Let of LBox<string>*expr*LBox<expr>
  | TypedLet of LBox<lltype*string>*expr*LBox<expr>
  | Quote of expr               // may not need for now
  | Setq of LBox<string>*expr   // destructive assignment
  | Sequence of LBox<expr> list  // includes function application (f x y z)
  // may want to change above for a more standard case for function application:
  // | Apply of expr*(expr list)
  | Beginseq of LBox<expr> list  // sequence of expressions
  // type expressions  
  | TypeExpr of lltype
  | Typedval of (lltype*expr)
  | Label of string   // not a proper expression - just a temporary
  | StrList of string list //Intermediate Addition
  | TypedStrList of (lltype*string) list //Intermediate
  | Error
  //  | Continuation of (expr -> expr)    // don't need

and lltype =  // abstract syntax for type expressions
  | LLint | LLfloat | LLstring
  | LList of lltype | LLtuple of lltype list
  | LLfun of (lltype list)*lltype
  | LLunknown | LLuntypable | LLvar of string | LLunit


// 2. Create a skeleton grammar with ast type and starting nonterminal (E)
let mutable Gmr = new_grammar<expr>("AxprList") 

// 3. Define terminal and non-terminal symbols (lexer interface)
let binops = ["+";"-";"*";"/";"%";"^";"=";"<";">";"<=";">=";"cons";"neq";"eq";"and";"or"]
let uniops = ["~";"car";"cdr";"not";"display"]
let keywords = ["NIL";"int";"unit";"float";"string";"unit";"let";"define";"begin";"lambda";"while";"setq";"if"] 
let terminals = List.append (List.append keywords binops) uniops 
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
Gmr.nonterminals(["VarTypeplus";"VarTypelist";"Expr";"Seq";"Binop";"Uniop";"Axpr";"Typeopt";"Txpr";"VarTypeopt";"Strlist";"Sval";"Seq"]);  // these are in addition to AxprList

// 4. Create a lexical scanner for the language such as in the form of
//    a .lex file.  We will just use the ready-made  ll1_lex.dll  here.

// 5. Write a semantic action for each rule, and add productions with
//    semantic actions to the grammar. *****

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

//#Uniop and Binop

let semact2 (rhs:Vec<Stackitem<expr>>) =  
  let strval = 
    match (rhs.[0].value) with
      | Label(a) -> a
      | _ -> ""
  if strval = "" then Error
  else Binop(strval, rhs.[1].value, rhs.[2].value)
Gmr.production("Expr --> Binop Axpr Axpr", semact2)

let semact3 (rhs:Vec<Stackitem<expr>>) =  
  let strval = 
    match (rhs.[0].value) with
      | Label(a) -> a
      | _ -> ""
  if strval = "" then Error
  else Uniop(strval, rhs.[1].value)
Gmr.production("Expr --> Uniop Axpr", semact3)

Gmr.production("Binop --> +", fun r -> Label("+"))
Gmr.production("Binop --> -", fun r -> Label("-"))
Gmr.production("Binop --> *", fun r -> Label("*"))
Gmr.production("Binop --> /", fun r -> Label("/"))
Gmr.production("Binop --> %", fun r -> Label("%"))
Gmr.production("Binop --> ^", fun r -> Label("^"))
Gmr.production("Binop --> =", fun r -> Label("="))
Gmr.production("Binop --> <", fun r -> Label("<"))
Gmr.production("Binop --> >", fun r -> Label(">"))
Gmr.production("Binop --> <=", fun r -> Label("<="))
Gmr.production("Binop --> >=", fun r -> Label(">="))
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

Gmr.production("Expr --> if Axpr Axpr Axpr ", fun r -> Ifelse(r.[1].value, r.[2].value, r.[3].value))
Gmr.production("Expr --> while Axpr Axpr", fun r -> Whileloop(r.[1].value, r.[2].value))

//Types
Gmr.production("Txpr --> int", fun r -> TypeExpr(LLint))
Gmr.production("Txpr --> unit", fun r -> TypeExpr(LLunit))
Gmr.production("Txpr --> float", fun r -> TypeExpr(LLfloat))
Gmr.production("Txpr --> string", fun r -> TypeExpr(LLstring))
Gmr.production("Typeopt --> COLON Txpr", fun r -> r.[1].value)
Gmr.production("Typeopt --> ", fun r -> TypeExpr(LLunknown))

let semact4 (rhs:Vec<Stackitem<expr>>) =  
    match (rhs.[0].value, rhs.[1].value) with
      | (Var(a), TypeExpr(LLunknown)) -> Var(a)
      | (Var(a), TypeExpr(x)) -> TypedVar(x,a)
      | _ -> Error
Gmr.production("VarTypeopt --> VAR Typeopt", semact4)

let semact5 (rhs:Vec<Stackitem<expr>>) =  
    match (rhs.[1].value) with
      | Var(s) -> 
          let varbox = rhs.[1].tolbox(s) in Define(varbox, rhs.[2].value)
      | TypedVar(a,s) -> 
          let tvarbox = rhs.[1].tolbox(a,s) in TypedDefine(tvarbox, rhs.[2].value)
      | _ -> Error
Gmr.production("Expr --> define VarTypeopt Axpr", semact5)

let semact6 (rhs:Vec<Stackitem<expr>>) =  
  match (rhs.[0].value, rhs.[1].value) with
    | (Var(a), StrList([""])) -> StrList(a::[])
    | (TypedVar(a,b), StrList([""])) -> TypedStrList((a,b)::[])
    | (Var(a), StrList(l)) -> StrList(a::l)
    | (TypedVar(a,b), TypedStrList(l)) -> TypedStrList((a,b)::l)
    | _ -> Error
Gmr.production("Sval --> ", fun r -> StrList([""]))
Gmr.production("Sval --> VarTypeopt Sval", semact6)
Gmr.production("Strlist --> VarTypeopt Sval", semact6)


//#Let, Setq

let semact7 (rhs:Vec<Stackitem<expr>>) =  
    match (rhs.[2].value) with
      | Var(s) -> 
          let varbox = rhs.[2].tolbox(s) 
          let exprbox = rhs.[5].tolbox(rhs.[5].value)
          Let(varbox, rhs.[3].value, exprbox)
      | TypedVar(a,s) -> 
          let tvarbox = rhs.[2].tolbox(a,s) 
          let exprbox = rhs.[5].tolbox(rhs.[5].value)
          TypedLet(tvarbox, rhs.[3].value, exprbox)
      | _ -> Error
Gmr.production("Expr --> let LPAREN VarTypeopt Axpr RPAREN Axpr", semact7)


let semact8 (rhs:Vec<Stackitem<expr>>) = 
  match (rhs.[1].value) with
    | Var(s) -> let varbox = rhs.[1].tolbox(s) in Setq(varbox, rhs.[2].value)
    | _ -> Error
Gmr.production("Expr --> setq VAR Axpr ", semact8)


//#Sequences

Gmr.production("Seq --> Axpr Seq", semact1)
Gmr.production("Seq --> ", fun r -> Nil)

Gmr.production("Expr --> Seq", fun r -> r.[0].value)

let semact9 (rhs:Vec<Stackitem<expr>>) =  
  match (rhs.[1].value, rhs.[2].value) with
    | (a, Nil) -> let box = rhs.[1].tolbox(a) in Beginseq(box::[])
    | (a, Sequence(b)) -> let box = rhs.[1].tolbox(a) in Beginseq(box::b)
    | _ -> Error
Gmr.production("Expr --> begin Axpr Seq", semact9)

let semact10 (rhs:Vec<Stackitem<expr>>) =  
  match (rhs.[2].value, rhs.[4].value) with
    | (StrList(a), TypeExpr(LLunknown))-> Lambda(a, rhs.[5].value)
    | (TypedStrList(a), TypeExpr(x)) -> TypedLambda(a, x, rhs.[5].value)
    | _ -> Error
Gmr.production("Expr --> lambda LPAREN Strlist RPAREN Typeopt Axpr", semact10)


// 6. Create options to generate parse table or load it from json:

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

// 7. Create "main", possibly in a different file, and start parsing ...
