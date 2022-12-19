target triple = "x86_64-pc-linux-gnu"
declare void @free(i8*)
declare i8* @malloc(i64)
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
declare void @check_index(i32, i32, i32)
define i32 @main() {
beginmain:
ret i32 0
}
