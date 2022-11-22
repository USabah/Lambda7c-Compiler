target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline(i8*)
declare i32 @lambda7c_cin()
@str_1 = constant [8 x i8] c"string\0a\00"
define i32 @main(){
beginmain:
%r_1 = getelementptr inbounds [8 x i8], [8 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_1)
ret i32 0
}
