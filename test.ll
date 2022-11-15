target triple = "x86_64-pc-linux-gnu"
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline(i8*)
@str_1 = constant [5 x i8] c"true\00"
@str_2 = constant [6 x i8] c"false\00"
define i32 @main(){
beginmain:
%r_1 = trunc i32 1 to i1
br i1 %r_1, label %iftrue_2, label %iffalse_3
iftrue_2:
br label %endif_4
iffalse_3:
br label %endif_4
endif_4:
%r_5 = phi i32 [0, %iftrue_2], [0, %iffalse_3]
%r_6 = trunc i32 %r_5 to i1
br i1 %r_6, label %iftrue_7, label %iffalse_8
iftrue_7:
%r_10 = getelementptr inbounds [5 x i8], [5 x i8]* @str_1, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_10)
br label %endif_9
iffalse_8:
%r_11 = getelementptr inbounds [6 x i8], [6 x i8]* @str_2, i64 0, i64 0
call void @lambda7c_printstr(i8* %r_11)
br label %endif_9
endif_9:
ret i32 0
}
