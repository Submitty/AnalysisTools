%option yylineno

O   [0-7]
D   [0-9]
NZ  [1-9]
L   [a-zA-Z_]
A   [a-zA-Z_0-9]
H   [a-fA-F0-9]
HP  (0[xX])
E   ([Ee][+-]?{D}+)
P   ([Pp][+-]?{D}+)
FS  (f|F|l|L)
IS  (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))
CP  (u|U|L)
SP  (u8|u|U|L)
ES  (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
WS  [ \t\v\n\f]

%x COMMENTSTATE

%{
#include <stdio.h>
#include "tokens.h"
%}

%%
"/*"					{ BEGIN(COMMENTSTATE); }
<COMMENTSTATE>"*/"			{ BEGIN(INITIAL); }
<COMMENTSTATE>\n			{ }
<COMMENTSTATE>.				{ }
"//".*                                  { }

"auto"					{ return AUTO; }
"break"					{ return BREAK; }
"case"					{ return CASE; }
"char"					{ return CHAR; }
"const"					{ return CONST; }
"continue"				{ return CONTINUE; }
"default"				{ return DEFAULT; }
"do"					{ return DO; }
"double"				{ return DOUBLE; }
"else"					{ return ELSE; }
"enum"					{ return ENUM; }
"extern"				{ return EXTERN; }
"float"					{ return FLOAT; }
"for"					{ return FOR; }
"goto"					{ return GOTO; }
"if"					{ return IF; }
"inline"				{ return INLINE; }
"int"					{ return INT; }
"long"					{ return LONG; }
"register"				{ return REGISTER; }
"restrict"				{ return RESTRICT; }
"return"				{ return RETURN; }
"short"					{ return SHORT; }
"signed"				{ return SIGNED; }
"sizeof"				{ return SIZEOF; }
"static"				{ return STATIC; }
"struct"				{ return STRUCT; }
"switch"				{ return SWITCH; }
"typedef"				{ return TYPEDEF; }
"union"					{ return UNION; }
"unsigned"				{ return UNSIGNED; }
"void"					{ return VOID; }
"volatile"				{ return VOLATILE; }
"while"					{ return WHILE; }
"_Alignas"                              { return ALIGNAS; }
"_Alignof"                              { return ALIGNOF; }
"_Atomic"                               { return ATOMIC; }
"_Bool"                                 { return BOOL; }
"_Complex"                              { return COMPLEX; }
"_Generic"                              { return GENERIC; }
"_Imaginary"                            { return IMAGINARY; }
"_Noreturn"                             { return NORETURN; }
"_Static_assert"                        { return STATIC_ASSERT; }
"_Thread_local"                         { return THREAD_LOCAL; }
"__func__"                              { return FUNC_NAME; }

{L}{A}*					{ return IDENTIFIER; }

{HP}{H}+{IS}?				{ return INTEGER_LITERAL; }
{NZ}{D}*{IS}?				{ return INTEGER_LITERAL; }
"0"{O}*{IS}?				{ return INTEGER_LITERAL; }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ return INTEGER_LITERAL; }

{D}+{E}{FS}?				{ return FLOAT_LITERAL; }
{D}*"."{D}+{E}?{FS}?			{ return FLOAT_LITERAL; }
{D}+"."{E}?{FS}?			{ return FLOAT_LITERAL; }
{HP}{H}+{P}{FS}?			{ return FLOAT_LITERAL; }
{HP}{H}*"."{H}+{P}{FS}?			{ return FLOAT_LITERAL; }
{HP}{H}+"."{P}{FS}?			{ return FLOAT_LITERAL; }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ return STRING_LITERAL; }

"..."					{ return ELLIPSIS; }
">>="					{ return RIGHT_ASSIGN; }
"<<="					{ return LEFT_ASSIGN; }
"+="					{ return ADD_ASSIGN; }
"-="					{ return SUB_ASSIGN; }
"*="					{ return MUL_ASSIGN; }
"/="					{ return DIV_ASSIGN; }
"%="					{ return MOD_ASSIGN; }
"&="					{ return AND_ASSIGN; }
"^="					{ return XOR_ASSIGN; }
"|="					{ return OR_ASSIGN; }
">>"					{ return RIGHT_OP; }
"<<"					{ return LEFT_OP; }
"++"					{ return INC_OP; }
"--"					{ return DEC_OP; }
"->"					{ return PTR_OP; }
"&&"					{ return AND_OP; }
"||"					{ return OR_OP; }
"<="					{ return LE_OP; }
">="					{ return GE_OP; }
"=="					{ return EQ_OP; }
"!="					{ return NE_OP; }
";"					{ return SEMICOLON; }
("{"|"<%")				{ return LEFT_CURLY; }
("}"|"%>")				{ return RIGHT_CURLY; }
","					{ return COMMA; }
":"					{ return COLON; }
"="					{ return EQUAL; }
"("					{ return LEFT_PAREN; }
")"					{ return RIGHT_PAREN; }
("["|"<:")				{ return LEFT_SQUARE; }
("]"|":>")				{ return RIGHT_SQUARE; }
"."					{ return DOT; }
"&"					{ return AMPERSAND; }
"!"					{ return EXCLAMATION; }
"~"					{ return TILDE; }
"-"					{ return MINUS; }
"+"					{ return PLUS; }
"*"					{ return ASTERISK; }
"/"					{ return SLASH; }
"%"					{ return PERCENT; }
"<"					{ return LESS_THAN; }
">"					{ return GREATER_THAN; }
"^"					{ return CARET; }
"|"					{ return PIPE; }
"?"					{ return QUESTION; }

{WS}+					{ }
.					{ }

%%

int main() {
	token c;
	while (c = yylex()) {
		printf("%d %d ", c, yylineno);
	}
}
