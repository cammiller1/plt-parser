array x [int 4];
int i;
int c;
for (i = 0 ; i < 4 ; i = i + 1) {
	x[i] = i;
	c = x[i];
	print(c);
}