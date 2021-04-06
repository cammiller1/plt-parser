## testing functions that have boolean values as arguments and if-else control flow ##

def void foo() {}

def int bar(int a, bool b, int c) { 
	if (b) {
		return c - a;
	} else {
		return a + c;
	}
}

print(bar(8, False, 10));