target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
define i32 @main() {
beginmain:
call void @lambda7c_printint(i32 1)
ret i32 0
}
