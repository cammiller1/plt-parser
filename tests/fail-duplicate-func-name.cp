## ensure duplicate function names raise an error ##

def int add(int a, int b) {
  return a + b;
}

def int add(int a, int b) {
  return a + b;
}

add(2, 3);
add(3, 4);