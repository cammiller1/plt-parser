## test that the if, else if, else can go to a sub-branch ##

int x;
int y;
y = 3;
x = 1;
if (x > 10) {
	print("went to if branch");
}
else if (x == 1) {
	
	if (y == 3) {
		print("went to the inner if in the elif branch");
	}
}
else if (x == 2) {
	print("went to second elif branch");
} else {
	print("went to else branch");
}