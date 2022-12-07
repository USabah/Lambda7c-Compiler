target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
define double @area_23(double %farg_radius_22, double* %pi_14, double* %z_12) {
beginfun:
%radius_22 = alloca double
store double %farg_radius_22, double* %radius_22
%r_1 = load double, double* %radius_22
%r_2 = load double, double* %radius_22
%r_3 = load double, double* %pi_14
%r_4 = fmul double %r_2, %r_3
%r_5 = fmul double %r_1, %r_4
ret double %r_5
}
define void @intelligence_test_25(double %farg_x_24, double* %pi_14, double* %z_12) {
beginfun:
%x_24 = alloca double
store double %farg_x_24, double* %x_24
%r_6 = load double, double* %z_12
%pi_26 = alloca double
store double %r_6, double* %pi_26
%r_7 = load double, double* %x_24
%r_8 = call double @area_23(double %r_7, double* %z_12, double* %pi_14)
call void @lambda7c_printfloat(double %r_8)
ret void 
}
define void @mymain_13(double %farg_z_12) {
beginfun:
%z_12 = alloca double
store double %farg_z_12, double* %z_12
%pi_14 = alloca double
store double 3.1415927, double* %pi_14
call void @intelligence_test_25(double 51.0, double* %z_12, double* %pi_14)
ret void 
}
define i32 @main() {
beginmain:
call void @mymain_13(double 2.0)
ret i32 0
}
