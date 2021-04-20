## purpose of this test is to initialize a local var in a function ##

def int add_3_to(int a, int b) {
  int c = 3;
  return a + c;
}

int a;
a = add_3_to(39, 3);
print(a);