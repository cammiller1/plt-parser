; ModuleID = 'complyed'
source_filename = "complyed"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i8*, ...) {
entry:
  %x = alloca double, i32 3
  %1 = getelementptr double, double* %x, i32 1
  store double 2.300000e+00, double* %1
  %2 = getelementptr double, double* %x, i32 1
  %3 = load double, double* %2
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), double %3)
  ret i32 0
}
