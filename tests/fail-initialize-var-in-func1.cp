## purpose of this test is to initialize a local string var in a function ##

def string gen_str(int a) {
  string c = "hello";
  return c;
}

string s;
## the below should not fail ##
print(c);