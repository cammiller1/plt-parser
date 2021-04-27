## Check if a triangle of positive area is possible with the given angles ##
## This example highlights "dangling else", as well as AND, OR, and comparison operators ##

## Adapted from: ##
## https://www.geeksforgeeks.org/check-if-a-triangle-of-positive-area-is-possible-with-the-given-angles/ ##


def void isTriangleExists(int a, int b, int c)
{
    ## Checking if the sum of three ##
    ## angles is 180 and none of ##
    ## the angles is zero ##

    if(a != 0 and b != 0 and c != 0 and (a + b + c) == 180)
    {
        ## Checking if sum of any two angles ##
        ## is greater than equal to the third one ##

        if((a + b) >= c or (b + c) >= a or (a + c) >= b)
        {
            print("YES");
        }
        else
        {
            print("NO");
        }
    }
    else
    {
        print("NO");
    }
}
 

int a;
a = 50;
int b;
b = 60;
int c;
c = 70;

isTriangleExists(50, 60, 70);