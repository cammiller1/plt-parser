; ModuleID = 'complyed'
source_filename = "complyed"

@x = global double 0.000000e+00
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [6 x i8] c"%.2f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i8*, ...) {
entry:
  store double 4.560000e+00, double* @x
  %x = load double, double* @x
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @fmt.1, i32 0, i32 0), double %x)
  ret i32 0
}
