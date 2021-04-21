## testing non-global string intialization ##

def int foo(int a){
	string s = "local string object";
	print(s);
	return 1;
}

foo(3);