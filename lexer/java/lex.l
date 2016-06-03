%option yylineno

%{
#include <stdio.h>
#include "tokens.h"
%}

%%

[ \t\r\n]			{ }

"//".*				{ }
"/""*"([^*]|"*"*[^*/])*"*"+"/"	{ }

"abstract"			{ return ABSTRACT; }
"assert"			{ return ASSERT; }
"boolean"			{ return BOOLEAN; }
"break"				{ return BREAK; }
"byte"				{ return BYTE; }
"case"				{ return CASE; }
"catch"				{ return CATCH; }
"char"				{ return CHAR; }
"class"				{ return CLASS; }
"const"				{ return CONST; }
"continue"			{ return CONTINUE; }
"default"			{ return DEFAULT; }
"do"				{ return DO; }
"double"			{ return DOUBLE; }
"else"				{ return ELSE; }
"extends"			{ return EXTENDS; }
"finally"			{ return FINALLY; }
"float"				{ return FLOAT; }
"for"				{ return FOR; }
"goto"				{ return GOTO; }
"if"				{ return IF; }
"implements"			{ return IMPLEMENTS; }
"import"			{ return IMPORT; }
"instanceof"			{ return INSTANCEOF; }
"int"				{ return INT; }
"interface"			{ return INTERFACE; }
"long"				{ return LONG; }
"native"			{ return NATIVE; }
"new"				{ return NEW; }
"package"			{ return PACKAGE; }
"private"			{ return PRIVATE; }
"protected"			{ return PROTECTED; }
"public"			{ return PUBLIC; }
"return"			{ return RETURN; }
"static"			{ return STATIC; }
"strictfp"			{ return STRICTFP; }
"super"				{ return SUPER; }
"synchronized"			{ return SYNCHRONIZED; }
"switch"			{ return SWITCH; }
"this"				{ return THIS; }
"throw"				{ return THROW; }
"throws"			{ return THROWS; }
"transient"			{ return TRANSIENT; }
"try"				{ return TRY; }
"void"				{ return VOID; }
"volatile"			{ return VOLATILE; }
"while"				{ return WHILE; }

[a-zA-Z_][a-zA-Z0-9_]*		{ return IDENTIFIER; }
-?[0-9]+			{ return INTEGER_LITERAL; }
-?[0-9]+\.[0-9]+		{ return FLOAT_LITERAL; }
\"([^\\\"]|\\.)*\"		{ return STRING_LITERAL; }

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
">>>"				{ return THREERIGHT; }
"++"				{ return INC_OP; }
"--"				{ return DEC_OP; }
"&&"				{ return AND_OP; }
"||"				{ return OR_OP; }
"<="				{ return LE_OP; }
">="				{ return GE_OP; }
"=="				{ return EQ_OP; }
"!="				{ return NE_OP; }
";"				{ return SEMICOLON; }
"{"				{ return LEFT_CURLY; }
"}"				{ return RIGHT_CURLY; }
","				{ return COMMA; }
":"				{ return COLON; }
"="				{ return EQUAL; }
"("				{ return LEFT_PAREN; }
")"				{ return RIGHT_PAREN; }
"["				{ return LEFT_SQUARE; }
"]"				{ return RIGHT_SQUARE; }
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
