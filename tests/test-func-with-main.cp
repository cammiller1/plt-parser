## test for ensuring the user can still declare some main function ##

def int add(int x, int y)
{
  return x + y;
}

def int main()
{
  int a;
  a = add(1, 3);

  print(a);
  return 0;
}

main();