target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline(i8*)
declare i32 @lambda7c_cin()

@str_1 = constant [2 x i8] c"\0a\00"
define i32 @f_7(i32 %farg_y_6, i32* %x_1) {
beginfun:
%y_6 = alloca i32
store i32 %farg_y_6, i32* %y_6
%r_1 = load i32, i32* %x_1
%r_2 = load i32, i32* %y_6
%r_3 = add i32 %r_1, %r_2
ret i32 %r_3
}
define i32 @main() {
beginmain:
%x_1 = alloca i32
store i32 1, i32* %x_1
%x_5 = alloca i32
store i32 4, i32* %x_5
%r_4 = call i32 @f_7(i32 2, i32* %x_1)
call void @lambda7c_printint(i32 %r_4)
%r_5 = load i32, i32* %x_5
call void @lambda7c_printint(i32 %r_5)
%r_6 = load i32, i32* %x_1
call void @lambda7c_printint(i32 %r_6)
%r_7 = getelementptr inbounds [2 x i8], [2 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_7)
ret i32 0
}
