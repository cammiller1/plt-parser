## the purpose of this test is to ensure an array can be declared inside of a function ##
def int add(int a, int b) {
  array x [int 3];
  return a + b;
}

int a;
a = add(39, 3);
print(a);