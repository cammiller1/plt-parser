#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>


/*
 * String concatenation operator
 */

char *string_concat(const char *s1, const char *s2)
{
	char *result = malloc(strlen(s1) + strlen(s2) + 1);
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}


/*
 * Return the length of a given string
 */

int len(const char *s) {
    return strlen(s);
}


/*
 * String comparison
 */

// int string_equality(const char *s1, const char *s2)
// {
// 	int res =  strcmp(s1, s2);
// 	if (res == 0)   return 1;   // equality is true
// 	else            return 0;   // equality is false
// }

bool string_equality(const char *s1, const char *s2)
{
	int res =  strcmp(s1, s2);
	if (res == 0)   return true;   // equality is true
	else            return false;   // equality is false
}

