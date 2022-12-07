target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
@str_1 = constant [4 x i8] c"\0aA\0a\00"
@str_2 = constant [4 x i8] c"\0aB\0a\00"
@str_3 = constant [3 x i8] c"C\0a\00"
@str_4 = constant [3 x i8] c"D\0a\00"
define i32 @fib_6(i32 %farg_n_5, i32* %x_4) {
beginfun:
%n_5 = alloca i32
store i32 %farg_n_5, i32* %n_5
%r_1 = load i32, i32* %n_5
%r_2 = icmp sle i32 %r_1, 2
%r_3 = zext i1 %r_2 to i32
%r_4 = trunc i32 %r_3 to i1
br i1 %r_4, label %iftrue_5, label %iffalse_6
iftrue_5:
br label %endif_7
iffalse_6:
%r_8 = load i32, i32* %n_5
%r_9 = sub i32 %r_8, 1
%r_10 = call i32 @fib_6(i32 %r_9, i32* %x_4)
%r_11 = load i32, i32* %n_5
%r_12 = sub i32 %r_11, 2
%r_13 = call i32 @fib_6(i32 %r_12, i32* %x_4)
%r_14 = add i32 %r_10, %r_13
br label %endif_7
endif_7:
%r_15 = phi i32 [1, %iftrue_5], [%r_14, %iffalse_6]
ret i32 %r_15
}
define i32 @main() {
beginmain:
%x_4 = alloca i32
store i32 0, i32* %x_4
%r_16 = load i32, i32* %x_4
%r_17 = icmp sgt i32 %r_16, 0
%r_18 = zext i1 %r_17 to i32
%r_19 = trunc i32 %r_18 to i1
br i1 %r_19, label %iftrue_20, label %iffalse_21
iftrue_20:
%r_23 = load i32, i32* %x_4
%r_24 = sdiv i32 10, %r_23
%r_25 = icmp sgt i32 %r_24, 1
%r_26 = zext i1 %r_25 to i32
br label %endif_22
iffalse_21:
br label %endif_22
endif_22:
%r_27 = phi i32 [%r_26, %iftrue_20], [0, %iffalse_21]
%r_28 = trunc i32 %r_27 to i1
br i1 %r_28, label %iftrue_29, label %iffalse_30
iftrue_29:
%r_32 = getelementptr inbounds [4 x i8], [4 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_32)
br label %endif_31
iffalse_30:
%r_33 = getelementptr inbounds [4 x i8], [4 x i8]* @str_2, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_33)
br label %endif_31
endif_31:
%r_34 = load i32, i32* %x_4
%r_35 = icmp slt i32 %r_34, 1
%r_36 = zext i1 %r_35 to i32
%r_37 = trunc i32 %r_36 to i1
br i1 %r_37, label %iftrue_38, label %iffalse_39
iftrue_38:
br label %endif_40
iffalse_39:
%r_41 = load i32, i32* %x_4
%r_42 = call i32 @fib_6(i32 100, i32* %x_4)
%r_43 = icmp slt i32 %r_41, %r_42
%r_44 = zext i1 %r_43 to i32
br label %endif_40
endif_40:
%r_45 = phi i32 [1, %iftrue_38], [%r_44, %iffalse_39]
%r_46 = trunc i32 %r_45 to i1
br i1 %r_46, label %iftrue_47, label %iffalse_48
iftrue_47:
%r_50 = getelementptr inbounds [3 x i8], [3 x i8]* @str_3, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_50)
br label %endif_49
iffalse_48:
%r_51 = getelementptr inbounds [3 x i8], [3 x i8]* @str_4, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_51)
br label %endif_49
endif_49:
ret i32 0
}
