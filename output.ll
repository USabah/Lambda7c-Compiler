target triple = "x86_64-pc-linux-gnu"
declare void @free(i8*)
declare i8* @malloc(i64)
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
declare void @check_index(i32, i32, i32)
%struct.f_2 = type { i32 }
%struct.g_6 = type { %struct.f_2*, i32 }
define i32 @f_2(i32 %farg_y_10, %struct.f_2* %_self) {
beginfun:
%y_10 = alloca i32
store i32 %farg_y_10, i32* %y_10
%y_10_1 = load i32, i32* %y_10
%x_1_2 = getelementptr inbounds %struct.f_2, %struct.f_2* %_self, i32 0, i32 0
%x_1_3 = load i32, i32* %x_1_2
%r_4 = add i32 %y_10_1, %x_1_3
ret i32 %r_4
}
define i32 @g_6(i32 %farg_y_12, %struct.g_6* %_self) {
beginfun:
%y_12 = alloca i32
store i32 %farg_y_12, i32* %y_12
%f_2 = getelementptr inbounds %struct.g_6, %struct.g_6* %_self, i32 0, i32 0
%f_2_1 = alloca %struct.f_2**
store %struct.f_2** %f_2, %struct.f_2*** %f_2_1
%f_2_9 = load %struct.f_2*, %struct.f_2** %f_2
%y_12_10 = load i32, i32* %y_12
%r_11 = call i32 @f_2(i32 %y_12_10, %struct.f_2* %f_2_9)
%x_1_12 = getelementptr inbounds %struct.g_6, %struct.g_6* %_self, i32 0, i32 1
%x_1_13 = load i32, i32* %x_1_12
%r_14 = add i32 %r_11, %x_1_13
ret i32 %r_14
}
define i32 @main() {
beginmain:
%x_1 = alloca i32
store i32 1, i32* %x_1
%f_2_5 = call i8* @malloc(i64 8)
%f_2_6 = bitcast i8* %f_2_5 to %struct.f_2*
%x_1_7 = getelementptr inbounds %struct.f_2, %struct.f_2* %f_2_6, i32 0, i32 0
%x_1_8 = load i32, i32* %x_1
store i32 %x_1_8, i32* %x_1_7
%f_2 = alloca %struct.f_2*
store %struct.f_2* %f_2_6, %struct.f_2** %f_2
%g_6_15 = call i8* @malloc(i64 16)
%g_6_16 = bitcast i8* %g_6_15 to %struct.g_6*
%f_2_17 = getelementptr inbounds %struct.g_6, %struct.g_6* %g_6_16, i32 0, i32 0
%f_2_18 = load %struct.f_2*, %struct.f_2** %f_2
store %struct.f_2* %f_2_18, %struct.f_2** %f_2_17
%x_1_19 = getelementptr inbounds %struct.g_6, %struct.g_6* %g_6_16, i32 0, i32 1
%x_1_20 = load i32, i32* %x_1
store i32 %x_1_20, i32* %x_1_19
%g_6 = alloca %struct.g_6*
store %struct.g_6* %g_6_16, %struct.g_6** %g_6
%g_6_21 = load %struct.g_6*, %struct.g_6** %g_6
%r_22 = call i32 @g_6(i32 1, %struct.g_6* %g_6_21)
call void @lambda7c_printint(i32 %r_22)
ret i32 0
}
