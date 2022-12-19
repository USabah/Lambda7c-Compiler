target triple = "x86_64-pc-linux-gnu"
declare void @free(i8*)
declare i8* @malloc(i64)
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
%struct.transaction_5 = type { double }
%struct.make_account_1 = type {  }
@str_1 = constant [2 x i8] c"\0a\00"
%struct.transfer_14 = type {  }
define double @transaction_5(double %farg_amt_24, %struct.transaction_5* %_self) {
beginfun:
%amt_24 = alloca double
store double %farg_amt_24, double* %amt_24
%balance_20 = getelementptr inbounds %struct.transaction_5, %struct.transaction_5* %_self, i32 0, i32 0
%balance_20_2 = getelementptr inbounds %struct.transaction_5, %struct.transaction_5* %_self, i32 0, i32 0
%balance_20_3 = load double, double* %balance_20_2
%amt_24_4 = load double, double* %amt_24
%r_5 = fadd double %balance_20_3, %amt_24_4
store double %r_5, double* %balance_20
ret double %r_5
}
define %struct.transaction_5* @make_account_1(double %farg_init_18, %struct.make_account_1* %_self) {
beginfun:
%init_18 = alloca double
store double %farg_init_18, double* %init_18
%init_18_1 = load double, double* %init_18
%balance_20 = alloca double
store double %init_18_1, double* %balance_20
%transaction_5_6 = call i8* @malloc(i64 8)
%transaction_5_7 = bitcast i8* %transaction_5_6 to %struct.transaction_5*
%balance_20_8 = getelementptr inbounds %struct.transaction_5, %struct.transaction_5* %transaction_5_7, i32 0, i32 0
%balance_20_9 = load double, double* %balance_20
store double %balance_20_9, double* %balance_20_8
%transaction_5 = alloca %struct.transaction_5*
store %struct.transaction_5* %transaction_5_7, %struct.transaction_5** %transaction_5
%transaction_5_10 = load %struct.transaction_5*, %struct.transaction_5** %transaction_5
ret %struct.transaction_5* %transaction_5_10
}
define double @transfer_14(%struct.transaction_5* %farg_a_26, %struct.transaction_5* %farg_b_27, double %farg_amt_28, %struct.transfer_14* %_self) {
beginfun:
%a_26 = alloca %struct.transaction_5*
store %struct.transaction_5* %farg_a_26, %struct.transaction_5** %a_26
%b_27 = alloca %struct.transaction_5*
store %struct.transaction_5* %farg_b_27, %struct.transaction_5** %b_27
%amt_28 = alloca double
store double %farg_amt_28, double* %amt_28
%a_26_24 = load %struct.transaction_5*, %struct.transaction_5** %a_26
%amt_28_25 = load double, double* %amt_28
%r_26 = fneg double %amt_28_25
%r_27 = call double @transaction_5(double %r_26, %struct.transaction_5* %a_26_24)
%b_27_28 = load %struct.transaction_5*, %struct.transaction_5** %b_27
%amt_28_29 = load double, double* %amt_28
%r_30 = call double @transaction_5(double %amt_28_29, %struct.transaction_5* %b_27_28)
ret double %r_30
}
define i32 @main() {
beginmain:
%make_account_1_11 = call i8* @malloc(i64 0)
%make_account_1_12 = bitcast i8* %make_account_1_11 to %struct.make_account_1*
%make_account_1 = alloca %struct.make_account_1*
store %struct.make_account_1* %make_account_1_12, %struct.make_account_1** %make_account_1
%make_account_1_13 = load %struct.make_account_1*, %struct.make_account_1** %make_account_1
%r_14 = call %struct.transaction_5* @make_account_1(double 100.0, %struct.make_account_1* %make_account_1_13)
%myaccount_10 = alloca %struct.transaction_5*
store %struct.transaction_5* %r_14, %struct.transaction_5** %myaccount_10
%make_account_1_15 = load %struct.make_account_1*, %struct.make_account_1** %make_account_1
%r_16 = call %struct.transaction_5* @make_account_1(double 200.0, %struct.make_account_1* %make_account_1_15)
%youraccount_11 = alloca %struct.transaction_5*
store %struct.transaction_5* %r_16, %struct.transaction_5** %youraccount_11
%myaccount_10_17 = load %struct.transaction_5*, %struct.transaction_5** %myaccount_10
%r_18 = call double @transaction_5(double 150.0, %struct.transaction_5* %myaccount_10_17)
call void @lambda7c_printfloat(double %r_18)
%r_19 = getelementptr inbounds [2 x i8], [2 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_19)
%youraccount_11_20 = load %struct.transaction_5*, %struct.transaction_5** %youraccount_11
%r_21 = fneg double 25.0
%r_22 = call double @transaction_5(double %r_21, %struct.transaction_5* %youraccount_11_20)
call void @lambda7c_printfloat(double %r_22)
%r_23 = getelementptr inbounds [2 x i8], [2 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_23)
%transfer_14_31 = call i8* @malloc(i64 0)
%transfer_14_32 = bitcast i8* %transfer_14_31 to %struct.transfer_14*
%transfer_14 = alloca %struct.transfer_14*
store %struct.transfer_14* %transfer_14_32, %struct.transfer_14** %transfer_14
%transfer_14_33 = load %struct.transfer_14*, %struct.transfer_14** %transfer_14
%youraccount_11_34 = load %struct.transaction_5*, %struct.transaction_5** %youraccount_11
%myaccount_10_35 = load %struct.transaction_5*, %struct.transaction_5** %myaccount_10
%r_36 = call double @transfer_14(%struct.transaction_5* %myaccount_10_35, %struct.transaction_5* %youraccount_11_34, double 100.0, %struct.transfer_14* %transfer_14_33)
%myaccount_10_37 = load %struct.transaction_5*, %struct.transaction_5** %myaccount_10
%r_38 = call double @transaction_5(double 0.0, %struct.transaction_5* %myaccount_10_37)
call void @lambda7c_printfloat(double %r_38)
%r_39 = getelementptr inbounds [2 x i8], [2 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_39)
%youraccount_11_40 = load %struct.transaction_5*, %struct.transaction_5** %youraccount_11
%r_41 = call double @transaction_5(double 0.0, %struct.transaction_5* %youraccount_11_40)
call void @lambda7c_printfloat(double %r_41)
%r_42 = getelementptr inbounds [2 x i8], [2 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_42)
%myaccount_10_43 = load %struct.transaction_5*, %struct.transaction_5** %myaccount_10
%transaction_5_44 = bitcast %struct.transaction_5* %myaccount_10_43 to i8*
call void @free(i8* %transaction_5_44)
%youraccount_11_45 = load %struct.transaction_5*, %struct.transaction_5** %youraccount_11
%transaction_5_46 = bitcast %struct.transaction_5* %youraccount_11_45 to i8*
call void @free(i8* %transaction_5_46)
ret i32 0
}
