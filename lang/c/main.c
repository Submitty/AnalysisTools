#include <stdio.h>

extern int yylex();
extern int yylineno;

int main()
{
	int token;
	while ((token = yylex())) printf("%d %d ", token, yylineno);
}
