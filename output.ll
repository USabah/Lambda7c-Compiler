target triple = "x86_64-pc-linux-gnu"
declare void @free(i8*)
declare i8* @malloc(i64)
declare void @lambda7c_printint(i32)
declare void @lambda7c_printfloat(double)
declare void @lambda7c_printstr(i8*)
declare void @lambda7c_newline()
declare i32 @lambda7c_cin()
%struct.log2_1 = type {  }
define i32 @log2_1(i32 %farg_n_5, %struct.log2_1* %_self) {
beginfun:
%n_5 = alloca i32
store i32 %farg_n_5, i32* %n_5
%n_5_1 = load i32, i32* %n_5
%r_2 = icmp slt i32 %n_5_1, 2
%r_3 = zext i1 %r_2 to i32
%r_4 = trunc i32 %r_3 to i1
br i1 %r_4, label %iftrue_5, label %iffalse_6
iftrue_5:
br label %endif_7
iffalse_6:
%n_5_8 = load i32, i32* %n_5
%r_9 = sdiv i32 %n_5_8, 2
%r_10 = call i32 @log2_1(i32 %r_9, %struct.log2_1* %_self)
%r_11 = add i32 1, %r_10
br label %endif_7
endif_7:
%r_12 = phi i32 [0, %iftrue_5], [%r_11, %iffalse_6]
ret i32 %r_12
}
define i32 @main() {
beginmain:
%log2_1_13 = call i8* @malloc(i64 0)
%log2_1_14 = bitcast i8* %log2_1_13 to %struct.log2_1*
%log2_1 = alloca %struct.log2_1*
store %struct.log2_1* %log2_1_14, %struct.log2_1** %log2_1
%log2_1_15 = load %struct.log2_1*, %struct.log2_1** %log2_1
%r_16 = call i32 @log2_1(i32 512, %struct.log2_1* %log2_1_15)
call void @lambda7c_printint(i32 %r_16)
ret i32 0
}
