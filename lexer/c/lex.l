%option yylineno

%{
#include <stdio.h>
#include "symtab.h"
%}

%%

[ \t\r\n]			{ }

"#".*				{ }
"//".*				{ }
"/""*"([^*]|"*"*[^*/])*"*"+"/"	{ }

"auto"				{ return AUTO; }
"break"				{ return BREAK; }
"case"				{ return CASE; }
"char"				{ return CHAR; }
"const"				{ return CONST; }
"continue"			{ return CONTINUE; }
"default"			{ return DEFAULT; }
"do"				{ return DO; }
"double"			{ return DOUBLE; }
"else"				{ return ELSE; }
"enum"				{ return ENUM; }
"extern"			{ return EXTERN; }
"float"				{ return FLOAT; }
"for"				{ return FOR; }
"goto"				{ return GOTO; }
"if"				{ return IF; }
"int"				{ return INT; }
"long"				{ return LONG; }
"register"			{ return REGISTER; }
"return"			{ return RETURN; }
"short"				{ return SHORT; }
"signed"			{ return SIGNED; }
"sizeof"			{ return SIZEOF; }
"static"			{ return STATIC; }
"struct"			{ return STRUCT; }
"switch"			{ return SWITCH; }
"typedef"			{ return TYPEDEF; }
"union"				{ return UNION; }
"unsigned"			{ return UNSIGNED; }
"void"				{ return VOID; }
"volatile"			{ return VOLATILE; }
"while"				{ return WHILE; }

[a-zA-Z_][a-zA-Z0-9_]*		{ return IDENTIFIER; }
-?[0-9]+			{ return INTEGER_LITERAL; }
-?[0-9]+\.[0-9]+		{ return FLOAT_LITERAL; }
\"([^\\\"]|\\.)*\"		{ return STRING_LITERAL; }

"..."				{ return ELLIPSIS; }
">>="				{ return RIGHT_ASSIGN; }
"<<="				{ return LEFT_ASSIGN; }
"+="				{ return ADD_ASSIGN; }
"-="				{ return SUB_ASSIGN; }
"*="				{ return MUL_ASSIGN; }
"/="				{ return DIV_ASSIGN; }
"%="				{ return MOD_ASSIGN; }
"&="				{ return AND_ASSIGN; }
"^="				{ return XOR_ASSIGN; }
"|="				{ return OR_ASSIGN; }
">>"				{ return RIGHT_OP; }
"<<"				{ return LEFT_OP; }
"++"				{ return INC_OP; }
"--"				{ return DEC_OP; }
"->"				{ return PTR_OP; }
"&&"				{ return AND_OP; }
"||"				{ return OR_OP; }
"<="				{ return LE_OP; }
">="				{ return GE_OP; }
"=="				{ return EQ_OP; }
"!="				{ return NE_OP; }
";"				{ return SEMICOLON; }
("{"|"<%")			{ return LEFT_CURLY; }
("}"|"%>")			{ return RIGHT_CURLY; }
","				{ return COMMA; }
":"				{ return COLON; }
"="				{ return EQUAL; }
"("				{ return LEFT_PAREN; }
")"				{ return RIGHT_PAREN; }
("["|"<:")			{ return LEFT_SQUARE; }
("]"|":>")			{ return RIGHT_SQUARE; }
"."				{ return DOT; }
"&"				{ return AMPERSAND; }
"!"				{ return EXCLAMATION; }
"~"				{ return TILDE; }
"-"				{ return MINUS; }
"+"				{ return PLUS; }
"*"				{ return ASTERISK; }
"/"				{ return SLASH; }
"%"				{ return PERCENT; }
"<"				{ return LESS_THAN; }
">"				{ return GREATER_THAN; }
"^"				{ return CARET; }
"|"				{ return PIPE; }
"?"				{ return QUESTION; }
.				{ }

%%

int main() {
	token c;
	while (c = yylex()) {
		printf("%d %d ", c, yylineno);
	}
}
