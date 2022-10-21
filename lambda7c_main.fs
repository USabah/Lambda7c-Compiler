module lamdba7c
open System
open System.IO
open Fussless
open lambda7c
open Recless.Base


//fsharpc lambda7c_main.fs -r lambda7c_ast.dll -r schemer_lex.dll


let runInteractive() = 
  Console.Write("Enter Expression: ");
  let lexer1 = schemerlexer<unit>(Console.ReadLine());
  parser1.set_scanner lexer1;;

let runFileBased(filename) = 
  let lines = File.ReadAllText(filename)
  let lexer1 = schemerlexer<unit>(lines);
  parser1.set_scanner lexer1;;

if runfile = "" then runInteractive()
else 
  printfn "Parsing File %s:" runfile
  runFileBased(runfile)

let result = parser1.parse()
if not(parser1.errors) then
  printfn "%A" (result) 
