# plt-parser

Running docker images:

(micro C):
docker run --rm -it -v `pwd`:/home/microc -w=/home/microc columbiasedwards/plt

How to:

Pretting print the AST:
 ./microc.native -a <test file>
 example: "./microc.native -a tests/test-hello.mc"

 Example output:
 int main()
	{
	(void : print((int : 42)));
	(void : print((int : 71)));
	(void : print((int : 1)));
	return (int : 0);
	}



Printing the generated LLVM IR:
 ./microc.native -l <test file>
 example: "./microc.native -l tests/test-hello.mc"

 Example output: 

 ; ModuleID = 'MicroC'
source_filename = "MicroC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 42)
  %printf1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 71)
  %printf2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 1)
  ret i32 0
}