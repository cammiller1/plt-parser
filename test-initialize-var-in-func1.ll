; ModuleID = 'complyed'
source_filename = "complyed"

@a = global i32 0
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.4 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.5 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @add_3_to(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %b2 = alloca i32
  store i32 %b, i32* %b2
  %c = alloca i32
  %a3 = load i32, i32* %a1
  %c4 = load i32, i32* %c
  %tmp = add i32 %a3, %c4
  ret i32 %tmp
}

define i32 @main(i8*, ...) {
entry:
  %add_3_to_result = call i32 @add_3_to(i32 39, i32 3)
  store i32 %add_3_to_result, i32* @a
  %a = load i32, i32* @a
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i32 %a)
  ret i32 0
}
