## ensure that a function can be passed as an argument ##

def int add(int a, int b) {
  return a + b;
}

int a;
a = add(5, add(39, 3));
print(a);