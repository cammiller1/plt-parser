array x [int 4];
x[0] = 2;
x[1] = 4;
x[2] = 6;
x[3] = 8;
int i;
int c;
for (i = 0 ; i < 4 ; i = i + 1) {
	c = x[i];
	print(c);
}