target triple = "x86_64-pc-linux-gnu"
declare void @free(i8*)
declare i8* @malloc(i64)
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
declare void @check_index(i32, i32, i32)
%struct.area_5 = type { double }
%struct.intelligence_test_9 = type { %struct.area_5*, double }
%struct.mymain_1 = type {  }
define double @area_5(double %farg_radius_25, %struct.area_5* %_self) {
beginfun:
%radius_25 = alloca double
store double %farg_radius_25, double* %radius_25
%radius_25_1 = load double, double* %radius_25
%radius_25_2 = load double, double* %radius_25
%pi_17_3 = getelementptr inbounds %struct.area_5, %struct.area_5* %_self, i32 0, i32 0
%pi_17_4 = load double, double* %pi_17_3
%r_5 = fmul double %radius_25_2, %pi_17_4
%r_6 = fmul double %radius_25_1, %r_5
ret double %r_6
}
define void @intelligence_test_9(double %farg_x_27, %struct.intelligence_test_9* %_self) {
beginfun:
%x_27 = alloca double
store double %farg_x_27, double* %x_27
%area_5 = getelementptr inbounds %struct.intelligence_test_9, %struct.intelligence_test_9* %_self, i32 0, i32 0
%area_5_1 = alloca %struct.area_5**
store %struct.area_5** %area_5, %struct.area_5*** %area_5_1
%z_15_11 = getelementptr inbounds %struct.intelligence_test_9, %struct.intelligence_test_9* %_self, i32 0, i32 1
%z_15_12 = load double, double* %z_15_11
%pi_29 = alloca double
store double %z_15_12, double* %pi_29
%area_5_13 = load %struct.area_5*, %struct.area_5** %area_5
%x_27_14 = load double, double* %x_27
%r_15 = call double @area_5(double %x_27_14, %struct.area_5* %area_5_13)
call void @lambda7c_printfloat(double %r_15)
ret void 
}
define void @mymain_1(double %farg_z_15, %struct.mymain_1* %_self) {
beginfun:
%z_15 = alloca double
store double %farg_z_15, double* %z_15
%pi_17 = alloca double
store double 3.1415927, double* %pi_17
%area_5_7 = call i8* @malloc(i64 8)
%area_5_8 = bitcast i8* %area_5_7 to %struct.area_5*
%pi_17_9 = getelementptr inbounds %struct.area_5, %struct.area_5* %area_5_8, i32 0, i32 0
%pi_17_10 = load double, double* %pi_17
store double %pi_17_10, double* %pi_17_9
%area_5 = alloca %struct.area_5*
store %struct.area_5* %area_5_8, %struct.area_5** %area_5
%intelligence_test_9_16 = call i8* @malloc(i64 16)
%intelligence_test_9_17 = bitcast i8* %intelligence_test_9_16 to %struct.intelligence_test_9*
%area_5_18 = getelementptr inbounds %struct.intelligence_test_9, %struct.intelligence_test_9* %intelligence_test_9_17, i32 0, i32 0
%area_5_19 = load %struct.area_5*, %struct.area_5** %area_5
store %struct.area_5* %area_5_19, %struct.area_5** %area_5_18
%z_15_20 = getelementptr inbounds %struct.intelligence_test_9, %struct.intelligence_test_9* %intelligence_test_9_17, i32 0, i32 1
%z_15_21 = load double, double* %z_15
store double %z_15_21, double* %z_15_20
%intelligence_test_9 = alloca %struct.intelligence_test_9*
store %struct.intelligence_test_9* %intelligence_test_9_17, %struct.intelligence_test_9** %intelligence_test_9
%intelligence_test_9_22 = load %struct.intelligence_test_9*, %struct.intelligence_test_9** %intelligence_test_9
call void @intelligence_test_9(double 51.0, %struct.intelligence_test_9* %intelligence_test_9_22)
ret void 
}
define i32 @main() {
beginmain:
%mymain_1_23 = call i8* @malloc(i64 0)
%mymain_1_24 = bitcast i8* %mymain_1_23 to %struct.mymain_1*
%mymain_1 = alloca %struct.mymain_1*
store %struct.mymain_1* %mymain_1_24, %struct.mymain_1** %mymain_1
%mymain_1_25 = load %struct.mymain_1*, %struct.mymain_1** %mymain_1
call void @mymain_1(double 2.0, %struct.mymain_1* %mymain_1_25)
ret i32 0
}
