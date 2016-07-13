%{
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include "../ast_node.h"
int yydebug = 1;
%}

%parse-param {ast_node **root}

%token <node> IDENTIFIER STRING_LITERAL
%token I_CONSTANT F_CONSTANT FUNC_NAME SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN

%token TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token CONST RESTRICT VOLATILE
%token BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token COMPLEX IMAGINARY 
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%token SEMICOLON LEFT_CURLY RIGHT_CURLY COMMA COLON EQUAL LEFT_PAREN RIGHT_PAREN
%token LEFT_SQUARE RIGHT_SQUARE DOT AMPERSAND EXCLAMATION TILDE
%token MINUS PLUS ASTERISK SLASH PERCENT LESS_THAN GREATER_THAN
%token CARET PIPE QUESTION

%type <node> primary_expression constant string
%type <node> generic_selection generic_assoc_list generic_association
%type <node> postfix_expression argument_expression_list
%type <node> unary_expression cast_expression
%type <node> multiplicative_expression additive_expression shift_expression
%type <node> relational_expression equality_expression
%type <node> and_expression exclusive_or_expression inclusive_or_expression
%type <node> logical_and_expression logical_or_expression
%type <node> conditional_expression
%type <node> assignment_expression
%type <node> expression constant_expression
%type <node> declaration declaration_specifiers init_declarator_list init_declarator
%type <node> storage_class_specifier type_specifier
%type <node> struct_or_union_specifier struct_declaration_list
%type <node> struct_declaration specifier_qualifier_list
%type <node> struct_declarator struct_declarator_list
%type <node> enum_specifier enumerator_list enumerator
%type <node> atomic_type_specifier type_qualifier function_specifier alignment_specifier
%type <node> declarator direct_declarator pointer
%type <node> type_qualifier_list parameter_type_list parameter_list parameter_declaration
%type <node> identifier_list type_name abstract_declarator direct_abstract_declarator
%type <node> initializer initializer_list designation designator_list designator
%type <node> static_assert_declaration statement labeled_statement compound_statement
%type <node> block_item_list block_item expression_statement selection_statement
%type <node> iteration_statement jump_statement translation_unit external_declaration
%type <node> function_definition declaration_list

%type <str> unary_operator assignment_operator struct_or_union

%start program
%%

program : translation_unit { *root = $1; }

primary_expression
	: IDENTIFIER { $$ = make_ast_node("identifier", NULL, NULL); }
	| constant { $$ = $1; }
	| string { $$ = $1; } 
	| LEFT_PAREN expression RIGHT_PAREN { $$ = $2; }
	| generic_selection { $$ = $1; }
	;

constant
	: I_CONSTANT { $$ = make_ast_node("integer", NULL, NULL); }
	| F_CONSTANT { $$ = make_ast_node("float", NULL, NULL); }
	;

string
	: STRING_LITERAL { $$ = $1; }
	| FUNC_NAME { $$ = make_ast_node("__func__", NULL, NULL); }
	;

generic_selection
	: GENERIC LEFT_PAREN assignment_expression COMMA generic_assoc_list RIGHT_PAREN
	{ $$ = make_ast_node("generic_selection", $3, $5); }
	;

generic_assoc_list
	: generic_association { $$ = make_ast_node("generic_assoc_list", $1, NULL); }
	| generic_assoc_list ',' generic_association
	{ $$ = make_ast_node("generic_assoc_list", $1, $3); }
	;

generic_association
	: type_name COLON assignment_expression
	{ $$ = make_ast_node("generic_association", $1, $3); }
	| DEFAULT COLON assignment_expression
	{ $$ = make_ast_node("generic_association_default", NULL, $3); }
	;

postfix_expression
	: primary_expression { $$ = $1; }
	| postfix_expression LEFT_SQUARE expression RIGHT_SQUARE
	{ $$ = make_ast_node("array_access", $1, $3); }
	| postfix_expression LEFT_PAREN RIGHT_PAREN
	{ $$ = make_ast_node("func_call", $1, NULL); }
	| postfix_expression LEFT_PAREN argument_expression_list RIGHT_PAREN
	{ $$ = make_ast_node("func_call", $1, $3); }
	| postfix_expression DOT IDENTIFIER
	{ $$ = make_ast_node("element_access", $1, $3); }
	| postfix_expression PTR_OP IDENTIFIER
	{ $$ = make_ast_node("deref_element_access", $1, $3); }
	| postfix_expression INC_OP { $$ = make_ast_node("increment_post", $1, NULL); }
	| postfix_expression DEC_OP { $$ = make_ast_node("decrement_post", $1, NULL); }
	| LEFT_PAREN type_name RIGHT_PAREN LEFT_CURLY initializer_list RIGHT_CURLY
	{ $$ = make_ast_node("literal_struct", $2, $5); }
	| LEFT_PAREN type_name RIGHT_PAREN LEFT_CURLY initializer_list COMMA RIGHT_CURLY
	{ $$ = make_ast_node("literal_struct", $2, $5); }
	;

argument_expression_list
	: assignment_expression
	{ $$ = make_ast_node("argument_expr_list", $1, NULL); }
	| argument_expression_list ',' assignment_expression
	{ $$ = make_ast_node("argument_expr_list", $3, $1); }
	;

unary_expression
	: postfix_expression { $$ = $1; }
	| INC_OP unary_expression { $$ = make_ast_node("increment_pre", $2, NULL); }
	| DEC_OP unary_expression { $$ = make_ast_node("decrement_pre", $2, NULL); }
	| unary_operator cast_expression { $$ = make_ast_node($1, $2, NULL); }
	| SIZEOF unary_expression { $$ = make_ast_node("sizeof_expr", $2, NULL); }
	| SIZEOF LEFT_PAREN type_name RIGHT_PAREN { $$ = make_ast_node("sizeof_type", $3, NULL); }
	| ALIGNOF LEFT_PAREN type_name RIGHT_PAREN { $$ = make_ast_node("alignof", $3, NULL); }
	;

unary_operator
	: AMPERSAND { $$ = "reference"; }
	| ASTERISK { $$ = "dereference"; }
	| PLUS { $$ = "positive"; }
	| MINUS { $$ = "negative"; }
	| TILDE { $$ = "tilde"; }
	| EXCLAMATION { $$ = "not"; }
	;

cast_expression
	: unary_expression { $$ = $1; }
	| LEFT_PAREN type_name RIGHT_PAREN cast_expression { $$ = make_ast_node("cast", $2, $4); }
	;

multiplicative_expression
	: cast_expression { $$ = $1; }
	| multiplicative_expression ASTERISK cast_expression
	{ $$ = make_ast_node("multiply", $1, $3); }
	| multiplicative_expression SLASH cast_expression
	{ $$ = make_ast_node("divide", $1, $3); }
	| multiplicative_expression PERCENT cast_expression
	{ $$ = make_ast_node("modulo", $1, $3); }
	;

additive_expression
	: multiplicative_expression { $$ = $1; }
	| additive_expression PLUS multiplicative_expression
	{ $$ = make_ast_node("add", $1, $3); }
	| additive_expression MINUS multiplicative_expression
	{ $$ = make_ast_node("subtract", $1, $3); }
	;

shift_expression
	: additive_expression { $$ = $1; }
	| shift_expression LEFT_OP additive_expression
	{ $$ = make_ast_node("left_shift", $1, $3); }
	| shift_expression RIGHT_OP additive_expression
	{ $$ = make_ast_node("right_shift", $1, $3); }
	;

relational_expression
	: shift_expression { $$ = $1; }
	| relational_expression LESS_THAN shift_expression
	{ $$ = make_ast_node("less_than", $1, $3); }
	| relational_expression GREATER_THAN shift_expression
	{ $$ = make_ast_node("greater_than", $1, $3); }
	| relational_expression LE_OP shift_expression
	{ $$ = make_ast_node("less_equal", $1, $3); }
	| relational_expression GE_OP shift_expression
	{ $$ = make_ast_node("greater_equal", $1, $3); }
	;

equality_expression
	: relational_expression { $$ = $1; }
	| equality_expression EQ_OP relational_expression
	{ $$ = make_ast_node("equal", $1, $3); }
	| equality_expression NE_OP relational_expression
	{ $$ = make_ast_node("not_equal", $1, $3); }
	;

and_expression
	: equality_expression { $$ = $1; }
	| and_expression AMPERSAND equality_expression
	{ $$ = make_ast_node("bitwise_and", $1, $3); }
	;

exclusive_or_expression
	: and_expression { $$ = $1; }
	| exclusive_or_expression CARET and_expression
	{ $$ = make_ast_node("bitwise_xor", $1, $3); }
	;

inclusive_or_expression
	: exclusive_or_expression { $$ = $1; }
	| inclusive_or_expression PIPE exclusive_or_expression
	{ $$ = make_ast_node("bitwise_or", $1, $3); }
	;

logical_and_expression
	: inclusive_or_expression { $$ = $1; }
	| logical_and_expression AND_OP inclusive_or_expression
	{ $$ = make_ast_node("and", $1, $3); }
	;

logical_or_expression
	: logical_and_expression { $$ = $1; }
	| logical_or_expression OR_OP logical_and_expression
	{ $$ = make_ast_node("or", $1, $3); }
	;

conditional_expression
	: logical_or_expression { $$ = $1; }
	| logical_or_expression QUESTION expression COLON conditional_expression
	{ $$ = make_ast_node("ternary", $1, make_ast_node("ternary_body", $3, $5)); }
	;

assignment_expression
	: conditional_expression { $$ = $1; }
	| unary_expression assignment_operator assignment_expression
	{ $$ = make_ast_node($2, $1, $3); }
	;

assignment_operator
	: EQUAL { $$ = "assign"; }
	| MUL_ASSIGN { $$ = "mul_assign"; }
	| DIV_ASSIGN { $$ = "div_assign"; }
	| MOD_ASSIGN { $$ = "mod_assign"; }
	| ADD_ASSIGN { $$ = "add_assign"; }
	| SUB_ASSIGN { $$ = "sub_assign"; }
	| LEFT_ASSIGN { $$ = "left_assign"; }
	| RIGHT_ASSIGN { $$ = "right_assign"; }
	| AND_ASSIGN { $$ = "and_assign"; }
	| XOR_ASSIGN { $$ = "xor_assign"; }
	| OR_ASSIGN { $$ = "or_assign"; }
	;

expression
	: assignment_expression { $$ = $1; }
	| expression COMMA assignment_expression { $$ = make_ast_node("comma", $1, $3); }
	;

constant_expression
	: conditional_expression { $$ = $1; }
	;

declaration
	: declaration_specifiers SEMICOLON { $$ = make_ast_node("declaration", $1, NULL); }
	| declaration_specifiers init_declarator_list ';'
	{ $$ = make_ast_node("declaration", $1, $2); }
	| static_assert_declaration { $$ = $1; }
	;

declaration_specifiers
	: storage_class_specifier declaration_specifiers
	{$$ = make_ast_node("storage_class_specifier", $1, $2); }
	| storage_class_specifier
	{$$ = make_ast_node("storage_class_specifier", $1, NULL); }
	| type_specifier declaration_specifiers
	{$$ = make_ast_node("type_specifier", $1, $2); }
	| type_specifier
	{$$ = make_ast_node("type_specifier", $1, NULL); }
	| type_qualifier declaration_specifiers
	{$$ = make_ast_node("type_qualifier", $1, $2); }
	| type_qualifier
	{$$ = make_ast_node("type_qualifier", $1, NULL); }
	| function_specifier declaration_specifiers
	{$$ = make_ast_node("function_specifier", $1, $2); }
	| function_specifier
	{$$ = make_ast_node("function_specifier", $1, NULL); }
	| alignment_specifier declaration_specifiers
	{$$ = make_ast_node("alignment_specifier", $1, $2); }
	| alignment_specifier
	{$$ = make_ast_node("alignment_specifier", $1, NULL); }
	;

init_declarator_list
	: init_declarator { $$ = make_ast_node("init_declarator_list", $1, NULL); }
	| init_declarator_list COMMA init_declarator
	{ $$ = make_ast_node("init_declarator_list", $1, $3); }
	;

init_declarator
	: declarator EQUAL initializer { $$ = make_ast_node("init_declarator", $1, $3); }
	| declarator { $$ = make_ast_node("init_declarator", $1, NULL); }
	;

storage_class_specifier
	: TYPEDEF { $$ = make_ast_node("typedef", NULL, NULL); }
	| EXTERN { $$ = make_ast_node("extern", NULL, NULL); }
	| STATIC { $$ = make_ast_node("static", NULL, NULL); }
	| THREAD_LOCAL { $$ = make_ast_node("thread_local", NULL, NULL); }
	| AUTO { $$ = make_ast_node("auto", NULL, NULL); }
	| REGISTER { $$ = make_ast_node("register", NULL, NULL); }
	;

type_specifier
	: VOID { $$ = make_ast_node("void", NULL, NULL); }
	| CHAR { $$ = make_ast_node("char", NULL, NULL); }
	| SHORT { $$ = make_ast_node("short", NULL, NULL); }
	| INT { $$ = make_ast_node("int", NULL, NULL); }
	| LONG { $$ = make_ast_node("long", NULL, NULL); }
	| FLOAT { $$ = make_ast_node("float", NULL, NULL); }
	| DOUBLE { $$ = make_ast_node("double", NULL, NULL); }
	| SIGNED { $$ = make_ast_node("signed", NULL, NULL); }
	| UNSIGNED { $$ = make_ast_node("unsigned", NULL, NULL); }
	| BOOL { $$ = make_ast_node("bool", NULL, NULL); }
	| COMPLEX { $$ = make_ast_node("complex", NULL, NULL); }
	| IMAGINARY { $$ = make_ast_node("imaginary", NULL, NULL); }
	| atomic_type_specifier { $$ = $1; }
	| struct_or_union_specifier { $$ = $1; }
	| enum_specifier { $$ = $1; }
	;

struct_or_union_specifier
	: struct_or_union LEFT_CURLY struct_declaration_list RIGHT_CURLY
	{ $$ = make_ast_node($1, NULL, $3); }
	| struct_or_union IDENTIFIER LEFT_CURLY struct_declaration_list RIGHT_CURLY
	{ $$ = make_ast_node($1, $2, $4); }
	| struct_or_union IDENTIFIER
	{ $$ = make_ast_node($1, $2, NULL); }
	;

struct_or_union
	: STRUCT { $$ = "struct"; }
	| UNION { $$ = "union"; }
	;

struct_declaration_list
	: struct_declaration { $$ = make_ast_node("struct_declaration_list", $1, NULL); }
	| struct_declaration_list struct_declaration
	{ $$ = make_ast_node("struct_declaration_list", $1, $2); }
	;

struct_declaration
	: specifier_qualifier_list SEMICOLON
	{ $$ = make_ast_node("struct_declaration", $1, NULL); }
	| specifier_qualifier_list struct_declarator_list ';'
	{ $$ = make_ast_node("struct_declaration", $1, $2); }
	| static_assert_declaration { $$ = $1; }
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	{ $$ = make_ast_node("type_specifier", $1, $2); }
	| type_specifier
	{ $$ = make_ast_node("type_specifier", $1, NULL); }
	| type_qualifier specifier_qualifier_list
	{ $$ = make_ast_node("type_qualifier", $1, $2); }
	| type_qualifier
	{ $$ = make_ast_node("type_qualifier", $1, NULL); }
	;

struct_declarator_list
	: struct_declarator { $$ = make_ast_node("struct_declarator", $1, NULL); }
	| struct_declarator_list COMMA struct_declarator
	{ $$ = make_ast_node("struct_declarator", $1, $3); }
	;

struct_declarator
	: COLON constant_expression { $$ = make_ast_node("struct_declarator", NULL, $2); }
	| declarator COLON constant_expression
	{ $$ = make_ast_node("struct_declarator", $1, $3); }
	| declarator { $$ = make_ast_node("struct_declarator", $1, NULL); }
	;

enum_specifier
	: ENUM LEFT_CURLY enumerator_list RIGHT_CURLY { $$ = make_ast_node("enum", NULL, $3); }
	| ENUM LEFT_CURLY enumerator_list COMMA RIGHT_CURLY { $$ = make_ast_node("enum", NULL, $3); }
	| ENUM IDENTIFIER LEFT_CURLY enumerator_list RIGHT_CURLY { $$ = make_ast_node("enum", $2, $4); }
	| ENUM IDENTIFIER LEFT_CURLY enumerator_list COMMA RIGHT_CURLY { $$ = make_ast_node("enum", $2, $4); }
	| ENUM IDENTIFIER { $$ = make_ast_node("enum", $2, NULL); }
	;

enumerator_list
	: enumerator { $$ = make_ast_node("enum_list", $1, NULL); }
	| enumerator_list COMMA enumerator { $$ = make_ast_node("enum_list", $1, $3); }
	;

enumerator
	: IDENTIFIER EQUAL constant_expression { $$ = make_ast_node("enum_entry", $1, $3); }
	| IDENTIFIER { $$ = make_ast_node("enum_entry", $1, NULL); }
	;

atomic_type_specifier
	: ATOMIC LEFT_PAREN type_name RIGHT_PAREN { $$ = make_ast_node("atomic_spec", $3, NULL); }
	;

type_qualifier
	: CONST { $$ = make_ast_node("const", NULL, NULL); }
	| RESTRICT { $$ = make_ast_node("restrict", NULL, NULL); }
	| VOLATILE { $$ = make_ast_node("volatile", NULL, NULL); }
	| ATOMIC { $$ = make_ast_node("atomic", NULL, NULL); }
	;

function_specifier
	: INLINE { $$ = make_ast_node("inline", NULL, NULL); }
	| NORETURN { $$ = make_ast_node("noreturn", NULL, NULL); }
	;

alignment_specifier
	: ALIGNAS LEFT_PAREN type_name RIGHT_PAREN { $$ = make_ast_node("alignas", $3, NULL); }
	| ALIGNAS LEFT_PAREN constant_expression RIGHT_PAREN { $$ = make_ast_node("alignas", $3, NULL); }
	;

declarator
	: pointer direct_declarator { $$ = make_ast_node("declarator", $1, $2); }
	| direct_declarator { $$ = make_ast_node("declarator", $1, NULL); }
	;

direct_declarator
	: IDENTIFIER { $$ = $1; }
	| LEFT_PAREN declarator RIGHT_PAREN { $$ = $2; }
	| direct_declarator LEFT_SQUARE RIGHT_SQUARE { $$ = make_ast_node("array", $1, NULL); }
	| direct_declarator LEFT_SQUARE ASTERISK RIGHT_SQUARE { $$ = make_ast_node("array_star", $1, NULL); }
	| direct_declarator LEFT_SQUARE STATIC type_qualifier_list assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("array", $1,
	make_ast_node("array_static_body", $4, $5)); }
	| direct_declarator LEFT_SQUARE STATIC assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("array", $1,
	make_ast_node("array_static_body", NULL, $4)); }
	| direct_declarator LEFT_SQUARE type_qualifier_list ASTERISK RIGHT_SQUARE
	{ $$ = make_ast_node("array", $1,
	make_ast_node("array_qual_star_body", $3, NULL)); }
	| direct_declarator LEFT_SQUARE type_qualifier_list STATIC assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("array", $1,
	make_ast_node("array_static_body", $3, $5)); }
	| direct_declarator LEFT_SQUARE type_qualifier_list assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("array", $1,
	make_ast_node("array_qual_body", $3, $4)); }
	| direct_declarator LEFT_SQUARE type_qualifier_list RIGHT_SQUARE
	{ $$ = make_ast_node("array", $1,
	make_ast_node("array_qual_body", $3, NULL)); }
	| direct_declarator LEFT_SQUARE assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("array", $1, $3); }
	| direct_declarator LEFT_PAREN parameter_type_list RIGHT_PAREN
	{ $$ = make_ast_node("funcptr", $1, $3); }
	| direct_declarator LEFT_PAREN RIGHT_PAREN
	{ $$ = make_ast_node("funcptr", $1, NULL); }
	| direct_declarator LEFT_PAREN identifier_list RIGHT_PAREN
	{ $$ = make_ast_node("id_list", $1, $3); }
	;

pointer
	: ASTERISK type_qualifier_list pointer { $$ = make_ast_node("pointer", $2, $3); }
	| ASTERISK type_qualifier_list { $$ = make_ast_node("pointer", $2, NULL); }
	| ASTERISK pointer { $$ = make_ast_node("pointer", NULL, $2); }
	| ASTERISK { $$ = make_ast_node("pointer", NULL, NULL); }
	;

type_qualifier_list
	: type_qualifier { $$ = make_ast_node("type_qualifier_list", $1, NULL); }
	| type_qualifier_list type_qualifier
	{ $$ = make_ast_node("type_qualifier_list", $1, $2); }
	;


parameter_type_list
	: parameter_list COMMA ELLIPSIS { $$ = make_ast_node("ellipsis_params", $1, NULL); }
	| parameter_list { $$ = $1; }
	;

parameter_list
	: parameter_declaration { $$ = make_ast_node("parameter_list", $1, NULL); }
	| parameter_list COMMA parameter_declaration
	{ $$ = make_ast_node("parameter_list", $1, $3); }
	;

parameter_declaration
	: declaration_specifiers declarator
	{ $$ = make_ast_node("parameter_declaration", $1, $2); }
	| declaration_specifiers abstract_declarator
	{ $$ = make_ast_node("parameter_declaration", $1, $2); }
	| declaration_specifiers
	{ $$ = make_ast_node("parameter_declaration", $1, NULL); }
	;

identifier_list
	: IDENTIFIER { $$ = make_ast_node("identifier_list", $1, NULL); }
	| identifier_list COMMA IDENTIFIER { $$ = make_ast_node("identifier_list", $3, $1); }
	;

type_name
	: specifier_qualifier_list abstract_declarator
	{ $$ = make_ast_node("typename", $1, $2); }
	| specifier_qualifier_list { $$ = make_ast_node("typename", $1, NULL); }
	;

abstract_declarator
	: pointer direct_abstract_declarator
	{ $$ = make_ast_node("abstract_declarator", $1, $2); }
	| pointer { $$ = $1; }
	| direct_abstract_declarator { $$ = $1; }
	;

direct_abstract_declarator
	: LEFT_PAREN abstract_declarator RIGHT_PAREN { $$ = make_ast_node("parens_declarator", $2, NULL); }
	| LEFT_SQUARE RIGHT_SQUARE { $$ = make_ast_node("braces", NULL, NULL); }
	| LEFT_SQUARE ASTERISK RIGHT_SQUARE { $$ = make_ast_node("braces_star", NULL, NULL); }
	| LEFT_SQUARE STATIC type_qualifier_list assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("braces_static_qual_assign", $3, $4); }
	| LEFT_SQUARE STATIC assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("braces_static_assign", NULL, $3); }
	| LEFT_SQUARE type_qualifier_list STATIC assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("braces_static_qual_assign", $2, $4); }
	| LEFT_SQUARE type_qualifier_list assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("braces_qual_assign", $2, $3); }
	| LEFT_SQUARE type_qualifier_list RIGHT_SQUARE
	{ $$ = make_ast_node("braces_qual", $2, NULL); }
	| LEFT_SQUARE assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("braces_assign", NULL, $2); }
	| direct_abstract_declarator LEFT_SQUARE RIGHT_SQUARE
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("braces", NULL, NULL)); }
	| direct_abstract_declarator LEFT_SQUARE ASTERISK RIGHT_SQUARE
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("braces_star", NULL, NULL)); }
	| direct_abstract_declarator LEFT_SQUARE STATIC type_qualifier_list assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("braces_static_qual_assign", $4, $5)); }
	| direct_abstract_declarator LEFT_SQUARE STATIC assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("braces_static_assign", NULL, $4)); }
	| direct_abstract_declarator LEFT_SQUARE type_qualifier_list assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("braces_qual_assign", $3, $4)); }
	| direct_abstract_declarator LEFT_SQUARE type_qualifier_list STATIC assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("braces_static_assign", NULL, $5)); }
	| direct_abstract_declarator LEFT_SQUARE type_qualifier_list RIGHT_SQUARE
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("braces_qual", $3, NULL)); }
	| direct_abstract_declarator LEFT_SQUARE assignment_expression RIGHT_SQUARE
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("braces_assign", $3, NULL)); }
	| LEFT_PAREN RIGHT_PAREN { $$ = make_ast_node("parens", NULL, NULL); }
	| LEFT_PAREN parameter_type_list RIGHT_PAREN { $$ = make_ast_node("parens", $2, NULL); }
	| direct_abstract_declarator LEFT_PAREN RIGHT_PAREN
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("parens", NULL, NULL)); }
	| direct_abstract_declarator LEFT_PAREN parameter_type_list RIGHT_PAREN
	{ $$ = make_ast_node("pre_abstract_declarator", $1,
	make_ast_node("parens", $3, NULL)); }
	;

initializer
	: LEFT_CURLY initializer_list RIGHT_CURLY { $$ = $2; }
	| LEFT_CURLY initializer_list COMMA RIGHT_CURLY { $$ = $2; }
	| assignment_expression { $$ = $1; }
	;

initializer_list
	: designation initializer
	{ $$ = make_ast_node("initializer_list",
	make_ast_node("designated", $1, $2), NULL); }
	| initializer
	{ $$ = make_ast_node("initializer_list", $1, NULL); }
	| initializer_list COMMA designation initializer
	{ $$ = make_ast_node("initializer_list",
	make_ast_node("designated", $3, $4), $1); }
	| initializer_list COMMA initializer
	{ $$ = make_ast_node("initializer_list", $3, $1); }
	;

designation
	: designator_list EQUAL { $$ = $1; }
	;

designator_list
	: designator { $$ = make_ast_node("designator_list", $1, NULL); }
	| designator_list designator { $$ = make_ast_node("designator_list", $2, $1); }
	;

designator
	: LEFT_SQUARE constant_expression RIGHT_SQUARE { $$ = $2; }
	| DOT IDENTIFIER { $$ = $2; }
	;

static_assert_declaration
	: STATIC_ASSERT LEFT_PAREN constant_expression COMMA STRING_LITERAL RIGHT_PAREN SEMICOLON
	{ $$ = make_ast_node("static_assert_declaration", $3, $5); }
	;

statement
	: labeled_statement { $$ = $1; }
	| compound_statement { $$ = $1; }
	| expression_statement { $$ = $1; }
	| selection_statement { $$ = $1; }
	| iteration_statement { $$ = $1; }
	| jump_statement { $$ = $1; }
	;

labeled_statement
	: IDENTIFIER COLON statement { $$ = make_ast_node("label", $1, $3); }
	| CASE constant_expression COLON statement { $$ = make_ast_node("case", $2, $4); }
	| DEFAULT COLON statement { $$ = make_ast_node("default", NULL, $3); }
	;

compound_statement
	: LEFT_CURLY RIGHT_CURLY { $$ = make_ast_node("block", NULL, NULL); }
	| LEFT_CURLY  block_item_list RIGHT_CURLY { $$ = make_ast_node("block", $2, NULL); }
	;

block_item_list
	: block_item { $$ = make_ast_node("block_items", $1, NULL); }
	| block_item_list block_item { $$ = make_ast_node("block_items", $2, $1); }
	;

block_item
	: declaration { $$ = $1; }
	| statement { $$ = $1; }
	;

expression_statement
	: SEMICOLON { $$ = make_ast_node("nop", NULL, NULL); }
	| expression SEMICOLON { $$ = $1; }
	;

selection_statement
	: IF LEFT_PAREN expression RIGHT_PAREN statement ELSE statement
	{ $$ = make_ast_node("if", $3, make_ast_node("if_body", $5, $7)); }
	| IF LEFT_PAREN expression RIGHT_PAREN statement
	{ $$ = make_ast_node("if", $3, make_ast_node("if_body", $5, NULL)); }
	| SWITCH LEFT_PAREN expression RIGHT_PAREN statement { $$ = make_ast_node("switch", $3, $5); }
	;

iteration_statement
	: WHILE LEFT_PAREN expression RIGHT_PAREN statement { $$ = make_ast_node("while", $3, $5); }
	| DO statement WHILE LEFT_PAREN expression RIGHT_PAREN SEMICOLON
	{ $$ = make_ast_node("dowhile", $5, $2); }
	| FOR LEFT_PAREN expression_statement expression_statement RIGHT_PAREN statement
	{ $$ = make_ast_node("for",
	make_ast_node("init_and_cond", $3, $4),
	make_ast_node("modify_and_body", NULL, $6)); }
	| FOR LEFT_PAREN expression_statement expression_statement expression RIGHT_PAREN statement
	{ $$ = make_ast_node("for",
	make_ast_node("init_and_cond", $3, $4),
	make_ast_node("modify_and_body", $5, $7)); }
	| FOR LEFT_PAREN declaration expression_statement RIGHT_PAREN statement
	{ $$ = make_ast_node("for",
	make_ast_node("init_and_cond", $3, $4),
	make_ast_node("modify_and_body", NULL, $6)); }
	| FOR LEFT_PAREN declaration expression_statement expression RIGHT_PAREN statement
	{ $$ = make_ast_node("for",
	make_ast_node("init_and_cond", $3, $4),
	make_ast_node("modify_and_body", $5, $7)); }
	;

jump_statement
	: GOTO IDENTIFIER SEMICOLON { $$ = make_ast_node("goto", $2, NULL); }
	| CONTINUE SEMICOLON { $$ = make_ast_node("continue", NULL, NULL); }
	| BREAK SEMICOLON { $$ = make_ast_node("break", NULL, NULL); }
	| RETURN SEMICOLON { $$ = make_ast_node("return", NULL, NULL); }
	| RETURN expression SEMICOLON { $$ = make_ast_node("return", $2, NULL); }
	;

translation_unit
	: external_declaration { $$ = make_ast_node("translation_unit", $1, NULL); }
	| translation_unit external_declaration
	{ $$ = make_ast_node("translation_unit", $2, $1); }
	;

external_declaration
	: function_definition { $$ = $1; }
	| declaration { $$ = $1; }
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
	{ $$ = make_ast_node("function_definition",
	make_ast_node("typespec", $1, $2),
	make_ast_node("decls_and_body", $3, $4)); }
	| declaration_specifiers declarator compound_statement
	{ $$ = make_ast_node("function_definition",
	make_ast_node("typespec", $1, $2),
	make_ast_node("decls_and_body", NULL, $3)); }
	;

declaration_list
	: declaration { $$ = make_ast_node("declaration_list", $1, NULL); }
	| declaration_list declaration
	{ $$ = make_ast_node("declaration_list", $2, $1); }
	;

%%
#include <stdio.h>

void yyerror(const char *s)
{
	fflush(stdout);
	fprintf(stderr, "*** %s\n", s);
}

