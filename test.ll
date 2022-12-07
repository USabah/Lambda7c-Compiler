target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()

define i32 @func_5(i32 %farg_z_4) {
beginfun:
%z_4 = alloca i32
store i32 %farg_z_4, i32* %z_4
%r_1 = load i32, i32* %z_4
ret i32 %r_1
}
define i32 @main() {
beginmain:
%r_2 = call i32 @func_5(i32 5)
call void @lambda7c_printint(i32 %r_2)
ret i32 0
}
