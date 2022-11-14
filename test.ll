target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline(i8*)
define i32 @main(){
beginmain:
%sum_1 = alloca i32
store i32 0, i32* %sum_1
%i_2 = alloca i32
store i32 1, i32* %i_2
br label %check_cond_1
check_cond_1:
%r_4 = load i32, i32* %i_2
%r_5 = icmp sle i32 %r_4, 100
br i1 %r_5, label %loop_2, label %endloop_3
loop_2:
%r_6 = load i32, i32* %sum_1
%r_7 = load i32, i32* %i_2
%r_8 = add i32 %r_6, %r_7
store i32 %r_8, i32* %sum_1
%r_9 = load i32, i32* %i_2
%r_10 = add i32 %r_9, 1
store i32 %r_10, i32* %i_2
br label %check_cond_1
endloop_3:
%r_11 = load i32, i32* %sum_1
call void @lambda7c_printint(i32 %r_11)
ret i32 0
}
