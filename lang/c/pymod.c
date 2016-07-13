#include "../ast_node.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <python3.4/Python.h>

extern int yyparse(ast_node **root);
extern FILE *yyin;

static PyObject *parse_c(PyObject *self, PyObject *args)
{
	/*char *str;
	if (!PyArg_ParseTuple(args, "s", str)) return NULL;
*/
	ast_node *root;
	//yy_scan_string(str);
	yyparse(&root);

	printf("type: %s\n", root->type);

	PyObject *ret = PyCapsule_New((void *) root, NULL, NULL);
	Py_INCREF(ret);
	return ret;
}

static PyMethodDef newc_methods[] = {
	{"parse_c", parse_c, METH_VARARGS, "Parse C program."},
	{NULL, NULL, 0, NULL}
};

static PyModuleDef newc_module = {
	PyModuleDef_HEAD_INIT,
	"newc",
	"docstring",
	-1,
	newc_methods
};

PyMODINIT_FUNC PyInit_newc() {
	PyObject *m = PyModule_Create(&newc_module);
	if (m == NULL) return NULL;

	return m;
}
