; ModuleID = 'complyed'
source_filename = "complyed"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@str = private unnamed_addr constant [6 x i8] c"hello\00"

declare i32 @printf(i8*, ...)

define i32 @main(i8*, ...) {
entry:
  %x = alloca i8*, i32 3
  %1 = getelementptr i8*, i8** %x, i32 1
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str, i32 0, i32 0), i8** %1
  %2 = getelementptr i8*, i8** %x, i32 1
  %3 = load i8*, i8** %2
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* %3)
  ret i32 0
}
