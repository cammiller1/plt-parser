## ensure string variables can be declared inside of a function ##

def string bar(int a, int b) { 
	string s;
	s = "I was declared and assigned in the functon";
	return s;
}

string a;
a = bar(1, 2);
print(a);