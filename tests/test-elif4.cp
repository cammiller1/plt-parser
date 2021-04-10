## test that the dangling else does not occur ##

int x;
int y;
y = 3;
x = 1;
if (x > 10) {
	print("went to if branch");
}
else if (x == 1) {

	if (y == 2) {
		print("went to the inner if in the elif branch");
	}
}
else if (x == 2) {
	print("went to second elif branch");
} else {
	print("went to else branch");
}