target triple = "x86_64-pc-linux-gnu"
declare void @free(i8*)
declare i8* @malloc(i64)
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
declare void @check_index(i32, i32, i32)
@str_1 = constant [64 x i8] c"Think of a number between 1 and 100 and I will try to guess it\0a\00"
@str_2 = constant [7 x i8] c" ... \0a\00"
@str_3 = constant [16 x i8] c"Is your number \00"
@str_4 = constant [51 x i8] c" (enter 0 if yes), or is it higher(1) or lower(2):\00"
@str_5 = constant [18 x i8] c"Invalid Response\0a\00"
@str_6 = constant [24 x i8] c"I guessed your number!\0a\00"
@str_7 = constant [63 x i8] c"YOU CHEATED! HUMANS CAN NEVER BE TRUSTED. ACTIVATING AREA 51!\0a\00"
define i32 @main() {
beginmain:
%r_1 = getelementptr inbounds [64 x i8], [64 x i8]* @str_1, i64 0, i32 0
call void @lambda7c_printstr(i8* %r_1)
%r_2 = getelementptr inbounds [7 x i8], [7 x i8]* @str_2, i64 0, i32 0
call void @lambda7c_printstr(i8* %r_2)
%min_1 = alloca i32
store i32 1, i32* %min_1
%max_2 = alloca i32
store i32 100, i32* %max_2
%guess_3 = alloca i32
store i32 0, i32* %guess_3
%response_4 = alloca i32
store i32 1, i32* %response_4
br label %check_cond_3
check_cond_3:
%min_1_6 = load i32, i32* %min_1
%max_2_7 = load i32, i32* %max_2
%r_8 = icmp sle i32 %min_1_6, %max_2_7
%r_9 = zext i1 %r_8 to i32
%r_10 = trunc i32 %r_9 to i1
br i1 %r_10, label %iftrue_11, label %iffalse_12
iftrue_11:
%response_4_14 = load i32, i32* %response_4
%r_15 = icmp ne i32 %response_4_14, 0
%r_16 = zext i1 %r_15 to i32
br label %endif_13
iffalse_12:
br label %endif_13
endif_13:
%r_17 = phi i32 [%r_16, %iftrue_11], [0, %iffalse_12]
%r_18 = trunc i32 %r_17 to i1
br i1 %r_18, label %loop_4, label %endloop_5
loop_4:
%min_1_19 = load i32, i32* %min_1
%max_2_20 = load i32, i32* %max_2
%r_21 = add i32 %min_1_19, %max_2_20
%r_22 = sdiv i32 %r_21, 2
store i32 %r_22, i32* %guess_3
%r_23 = getelementptr inbounds [16 x i8], [16 x i8]* @str_3, i64 0, i32 0
call void @lambda7c_printstr(i8* %r_23)
%guess_3_24 = load i32, i32* %guess_3
call void @lambda7c_printint(i32 %guess_3_24)
%r_25 = getelementptr inbounds [51 x i8], [51 x i8]* @str_4, i64 0, i32 0
call void @lambda7c_printstr(i8* %r_25)
%in_26 = call i32 @lambda7c_cin()
store i32 %in_26, i32* %response_4
%response_4_27 = load i32, i32* %response_4
%r_28 = icmp eq i32 %response_4_27, 1
%r_29 = zext i1 %r_28 to i32
%r_30 = trunc i32 %r_29 to i1
br i1 %r_30, label %iftrue_31, label %iffalse_32
iftrue_31:
%guess_3_34 = load i32, i32* %guess_3
%r_35 = add i32 %guess_3_34, 1
store i32 %r_35, i32* %min_1
br label %endif_33
iffalse_32:
%response_4_36 = load i32, i32* %response_4
%r_37 = icmp eq i32 %response_4_36, 2
%r_38 = zext i1 %r_37 to i32
%r_39 = trunc i32 %r_38 to i1
br i1 %r_39, label %iftrue_40, label %iffalse_41
iftrue_40:
%guess_3_43 = load i32, i32* %guess_3
%r_44 = sub i32 %guess_3_43, 1
store i32 %r_44, i32* %max_2
br label %endif_42
iffalse_41:
%response_4_45 = load i32, i32* %response_4
%r_46 = icmp eq i32 %response_4_45, 0
%r_47 = zext i1 %r_46 to i32
%r_48 = trunc i32 %r_47 to i1
br i1 %r_48, label %iftrue_49, label %iffalse_50
iftrue_49:
%r_52 = getelementptr inbounds [18 x i8], [18 x i8]* @str_5, i64 0, i32 0
call void @lambda7c_printstr(i8* %r_52)
br label %endif_51
iffalse_50:
br label %endif_51
endif_51:
%r_53 = phi i32 [0, %iftrue_49], [0, %iffalse_50]
br label %endif_42
endif_42:
%r_54 = phi i32 [%r_44, %iftrue_40], [%r_53, %endif_51]
br label %endif_33
endif_33:
%r_55 = phi i32 [%r_35, %iftrue_31], [%r_54, %endif_42]
br label %check_cond_3
endloop_5:
%response_4_56 = load i32, i32* %response_4
%r_57 = icmp eq i32 %response_4_56, 0
%r_58 = zext i1 %r_57 to i32
%r_59 = trunc i32 %r_58 to i1
br i1 %r_59, label %iftrue_60, label %iffalse_61
iftrue_60:
%r_63 = getelementptr inbounds [24 x i8], [24 x i8]* @str_6, i64 0, i32 0
call void @lambda7c_printstr(i8* %r_63)
br label %endif_62
iffalse_61:
%r_64 = getelementptr inbounds [63 x i8], [63 x i8]* @str_7, i64 0, i32 0
call void @lambda7c_printstr(i8* %r_64)
br label %endif_62
endif_62:
ret i32 0
}
