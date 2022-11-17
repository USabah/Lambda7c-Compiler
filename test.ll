target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline(i8*)
@str_1 = constant [4 x i8] c"   \00"
@str_2 = constant [3 x i8] c"  \00"
@str_3 = constant [2 x i8] c"\0a\00"
define i32 @main(){
beginmain:
%x_1 = alloca i32
store i32 1, i32* %x_1
%y_2 = alloca i32
store i32 0, i32* %y_2
br label %check_cond_1
check_cond_1:
%r_4 = load i32, i32* %x_1
%r_5 = icmp slt i32 %r_4, 10
%r_6 = zext i1 %r_5 to i32
%r_7 = trunc i32 %r_6 to i1
br i1 %r_7, label %loop_2, label %endloop_3
loop_2:
store i32 1, i32* %y_2
br label %check_cond_8
check_cond_8:
%r_11 = load i32, i32* %y_2
%r_12 = icmp slt i32 %r_11, 10
%r_13 = zext i1 %r_12 to i32
%r_14 = trunc i32 %r_13 to i1
br i1 %r_14, label %loop_9, label %endloop_10
loop_9:
%r_15 = load i32, i32* %x_1
%r_16 = load i32, i32* %y_2
%r_17 = mul i32 %r_15, %r_16
call void @lambda7c_printint(i32 %r_17)
%r_18 = load i32, i32* %x_1
%r_19 = load i32, i32* %y_2
%r_20 = mul i32 %r_18, %r_19
%r_21 = icmp slt i32 %r_20, 10
%r_22 = zext i1 %r_21 to i32
%r_23 = trunc i32 %r_22 to i1
br i1 %r_23, label %iftrue_24, label %iffalse_25
iftrue_24:
%r_27 = getelementptr inbounds [4 x i8], [4 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_27)
br label %endif_26
iffalse_25:
%r_28 = getelementptr inbounds [3 x i8], [3 x i8]* @str_2, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_28)
br label %endif_26
endif_26:
%r_29 = load i32, i32* %y_2
%r_30 = add i32 %r_29, 1
store i32 %r_30, i32* %y_2
br label %check_cond_8
endloop_10:
%r_31 = getelementptr inbounds [2 x i8], [2 x i8]* @str_3, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_31)
%r_32 = load i32, i32* %x_1
%r_33 = add i32 %r_32, 1
store i32 %r_33, i32* %x_1
br label %check_cond_1
endloop_3:
ret i32 0
}
