module lamdba7c
open System
open System.IO
open Fussless
open lambda7c
open Recless.Base


//fsharpc lambda7c_main.fs -r lambda7c_ast.dll -r lambda7c_typing.dll -r schemer_lex.dll -r llvm_compiler.dll


let runInteractive() = 
  Console.Write("Enter Expression: ");
  let lexer1 = schemerlexer<unit>(Console.ReadLine());
  //let lexer1 = lambda7clexer<unit>(Console.ReadLine());
  parser1.set_scanner lexer1;;

let runFileBased(filename) = 
  let lines = File.ReadAllText(filename)
  let lexer1 = schemerlexer<unit>(lines);
  //let lexer1 = lambda7clexer<unit>(lines);
  parser1.set_scanner lexer1;;

if runfile = "" then runInteractive()
else 
  printfn "Parsing File %s:" runfile
  runFileBased(runfile)

let result = parser1.parse()
if not(parser1.errors) then
  printfn "AST Representation------------" 
  printfn "%A" (result)
  
  printfn "\n\n\nType Checker--------------------"
  //wrap result in an LBox
  let lbox = new_stackitem("AxprList", result, 1, 1)
  let t = symbol_table.infer_type(lbox) 
  printfn "Type returned: %A" t
