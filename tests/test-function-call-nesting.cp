## to ensure functions call each other in the correct order ##

def void foo() {
	print("foo");

}

def void bar(int a, bool b, int c) { 
	print("bar");
	foo();
}

def void say_hello() {
	print("Hello world");
	bar(1, False, 2);
}

say_hello();