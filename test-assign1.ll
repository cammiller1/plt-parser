; ModuleID = 'complyed'
source_filename = "complyed"

@x = global i32 0
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i8*, ...) {
entry:
  %x = load i32, i32* @x
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %x)
  store i32 1, i32* @x
  ret i32 0
}
