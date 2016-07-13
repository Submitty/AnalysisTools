extern int yylex();

int main()
{
	int token;
	while ((token = yylex())) printf("%d ", token);
}
