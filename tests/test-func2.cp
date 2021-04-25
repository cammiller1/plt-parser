## purpose of this test is to show functions can be declared after calls ##
int a;
a = add(39, 3);
print(a);

def int add(int a, int b) {
  return a + b;
}