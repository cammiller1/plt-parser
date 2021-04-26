## show that functions can be defined in any order ##

def void say_hello() {
	print("Hello world");
	bar(1, False, 2);
}

def void bar(int a, bool b, int c) { 
	print("bar");
	foo();
}

def void foo() {
	print("foo");

}

say_hello();