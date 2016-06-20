#ifndef NODE_H
#define NODE_H

typedef struct ast_node {
	char type[32];
	struct ast_node *child1;
	struct ast_node *child2;
} ast_node;

typedef union YYSTYPE {
	ast_node *node;
	const char *str;
} YYSTYPE;

ast_node *make_ast_node(const char *type, ast_node *child1, ast_node *child2);

#endif
