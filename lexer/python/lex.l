%option yylineno

%{
#include <stdio.h>
#include "tokens.h"

#define START (continuing = 1)
#define STOP (continuing = 0)

int level = 0;
int indent_stack[72];
int continuing = 0;

token process_indent(char *line);
%}

%%

"#".*				{ }

^[ \t]*\n			{ /* blank line */ }
^[ ]*[^ \n]+			{
	int last = yyleng - 1;
	yylineno++;
	token res = process_indent(yytext);
	while ((last >= 0) && (yytext[last] != ' ')) {
		unput(yytext[last]);
		last--;
	}
	if (res != INVALID) {
		return res;
	}
}

"\\"				{ START; }

"False"				{ STOP; return FALSE; }
"None"				{ STOP; return NONE; }
"True"				{ STOP; return TRUE; }
"and"				{ STOP; return AND; }
"as"				{ STOP; return AS; }
"assert"			{ STOP; return ASSERT; }
"break"				{ STOP; return BREAK; }
"class"				{ STOP; return CLASS; }
"continue"			{ STOP; return CONTINUE; }
"def"				{ STOP; return DEF; }
"del"				{ STOP; return DEL; }
"elif"				{ STOP; return ELIF; }
"else"				{ STOP; return ELSE; }
"except"			{ STOP; return EXCEPT; }
"finally"			{ STOP; return FINALLY; }
"for"				{ STOP; return FOR; }
"from"				{ STOP; return FROM; }
"global"			{ STOP; return GLOBAL; }
"if"				{ STOP; return IF; }
"import"			{ STOP; return IMPORT; }
"is"				{ STOP; return IS; }
"lambda"			{ STOP; return LAMBDA; }
"nonlocal"			{ STOP; return NONLOCAL; }
"not"				{ STOP; return NOT; }
"or"				{ STOP; return OR; }
"pass"				{ STOP; return PASS; }
"raise"				{ STOP; return RAISE; }
"return"			{ STOP; return RETURN; }
"try"				{ STOP; return TRY; }
"while"				{ STOP; return WHILE; }
"with"				{ STOP; return WITH; }
"yield"				{ STOP; return YIELD; }

[a-zA-Z_][a-zA-Z0-9_]*			{ STOP; return IDENTIFIER; }
-?[0-9]+				{ STOP; return INTEGER_LITERAL; }
-?[0-9]+\.[0-9]+(j|J)			{ STOP; return IMAGINARY_LITERAL; }
-?[0-9]+\.[0-9]+(e|E)-?[0-9]+(j|J)	{ STOP; return IMAGINARY_LITERAL; }
-?[0-9]+(e|E)-?[0-9]+(j|J)		{ STOP; return IMAGINARY_LITERAL; }
-?[0-9]+\.[0-9]+			{ STOP; return FLOAT_LITERAL; }
-?[0-9]+\.[0-9]+(e|E)-?[0-9]+		{ STOP; return FLOAT_LITERAL; }
-?[0-9]+(e|E)-?[0-9]+			{ STOP; return FLOAT_LITERAL; }
\"([^\\\"]|\\.)*\"			{ STOP; return STRING_LITERAL; }
b\"([^\\\"]|\\.)*\"			{ STOP; return BYTES_LITERAL; }

"+"				{ STOP; return PLUS; }
"-"				{ STOP; return MINUS; }
"*"				{ STOP; return ASTERISK; }
"/"				{ STOP; return SLASH; }
"//"				{ STOP; return DOUBLE_SLASH; }
"%"				{ STOP; return PERCENT; }
"**"				{ STOP; return DOUBLE_ASTERISK; }
"=="				{ STOP; return EQ_OP; }
"!="				{ STOP; return NE_OP; }
"<"				{ STOP; return LESS_THAN; }
">"				{ STOP; return GREATER_THAN; }
"<="				{ STOP; return LE_OP; }
">="				{ STOP; return GE_OP; }
"&"				{ STOP; return AMPERSAND; }
"|"				{ STOP; return PIPE; }
"~"				{ STOP; return TILDE; }
"^"				{ STOP; return CARET; }
"<<"				{ STOP; return LEFT_OP; }
">>"				{ STOP; return RIGHT_OP; }
"("				{ STOP; return LEFT_PAREN; }
")"				{ STOP; return RIGHT_PAREN; }
"["				{ STOP; return LEFT_SQUARE; }
"]"				{ STOP; return RIGHT_SQUARE; }
"{"				{ STOP; return LEFT_CURLY; }
"}"				{ STOP; return RIGHT_CURLY; }
"."				{ STOP; return DOT; }
","				{ STOP; return COMMA; }
":"				{ STOP; return COLON; }
";"				{ STOP; return SEMICOLON; }
"@"				{ STOP; return AT; }
"="				{ STOP; return EQUAL; }
"+="				{ STOP; return ADD_ASSIGN; }
"-="				{ STOP; return SUB_ASSIGN; }
"*="				{ STOP; return MUL_ASSIGN; }
"/="				{ STOP; return DIV_ASSIGN; }
"//="				{ STOP; return INTDIV_ASSIGN; }
"%="				{ STOP; return MOD_ASSIGN; }
"**="				{ STOP; return POW_ASSIGN; }
"&="				{ STOP; return AND_ASSIGN; }
"|="				{ STOP; return OR_ASSIGN; }
"^="				{ STOP; return XOR_ASSIGN; }
"<<="				{ STOP; return LEFT_ASSIGN; }
">>="				{ STOP; return RIGHT_ASSIGN; }
.				{ }

%%

int white_count(char *line) { 
	int c;
	for (c = 0; *line == ' '; ++c, ++line);
	return c;
}

token process_indent(char *line)
{
	if (continuing) return INVALID;
	int indent = white_count(line);
	if (line[indent] == '#') return INVALID;
	if (indent > indent_stack[level]) {
		indent_stack[++level] = indent;
		return INDENT;
	}
	else if (indent < indent_stack[level]) {
		--level;
		return UNINDENT;
	}
	return INVALID;
}

int main()
{
	indent_stack[0] = 0;
	token c;
	while (c = yylex()) {
		printf("%d %d ", c, yylineno);
	}
}
