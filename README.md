# plt-parser

Running docker images:

(micro C):
docker run --rm -it -v `pwd`:/home/microc -w=/home/microc columbiasedwards/plt

How to:

Pretting print the AST:
 ./microc.native -a <test file>
 example: "./microc.native -a tests/test-hello.mc"

Printing the generated LLVM IR:
 ./microc.native -l <test file>
 example: "./microc.native -l tests/test-hello.mc"