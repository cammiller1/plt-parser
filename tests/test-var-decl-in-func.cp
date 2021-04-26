## ensure integer variables can be declared inside of a function ##

def int bar(int a, int b) { 
	int x;
	x = 1;
	return b - x;
}

print(bar(1, 2));