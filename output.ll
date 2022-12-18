target triple = "x86_64-pc-linux-gnu"
declare void @free(i8*)
declare i8* @malloc(i64)
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
%struct.x_1 = type {  }
@str_1 = constant [4 x i8] c"abc\00"
define void @x_1(i8* %farg_y_5, %struct.x_1* %_self) {
beginfun:
%y_5 = alloca i8*
store i8* %farg_y_5, i8** %y_5
%y_5_1 = load i8*, i8** %y_5
call void @lambda7c_printstr(i8* %y_5_1)
ret void 
}
define i32 @main() {
beginmain:
%x_1_2 = call i8* @malloc(i64 0)
%x_1_3 = bitcast i8* %x_1_2 to %struct.x_1*
%x_1 = alloca %struct.x_1*
store %struct.x_1* %x_1_3, %struct.x_1** %x_1
%x_1_4 = load %struct.x_1*, %struct.x_1** %x_1
%r_5 = getelementptr inbounds [4 x i8], [4 x i8]* @str_1, i64 0, i64 0
call void @x_1(i8* %r_5, %struct.x_1* %x_1_4)
ret i32 0
}
