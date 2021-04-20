## purpose of this test is to initialize a local var in a function ##

def int add_3_to(int a, int b) {
  int c = 10;
  return a + b + c;
}

int c;
c = add_3_to(39, 3);
print(c);