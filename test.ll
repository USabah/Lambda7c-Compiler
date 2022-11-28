target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline(i8*)
declare i32 @lambda7c_cin()

define double @area_1(double %farg_radius_3, double* %pi_1) {
beginfun:
%r_1 = load double, double* %radius_2
%r_2 = load double, double* %radius_2
%r_3 = load double, double* %pi_1
%r_4 = fmul double %r_2, %r_3
%r_5 = fmul double %r_1, %r_4
ret double %r_5
}
define i32 @main() {
beginmain:
%pi_1 = alloca double
store double 3.1415927, double* %pi_1
ret i32 0
}
