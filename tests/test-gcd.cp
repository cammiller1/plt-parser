## find the GCD ##
## https://www.geeksforgeeks.org/gcd-in-python/ ##

## Python code to demonstrate naive ##
## method to compute gcd ( recursion ) ##
  
def int hcfnaive(int a, int b) {
    if (b == 0) {
        return a;
    } else {
        return hcfnaive(b, a % b);
    }
}

print("The gcd of 60 and 48 is");
int result;
result = hcfnaive(60, 48);
print(result);