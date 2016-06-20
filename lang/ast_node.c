#include "ast_node.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <python3.4/Python.h>

ast_node *make_ast_node(const char *type, ast_node *child1, ast_node *child2)
{
	ast_node *ret = (ast_node *) malloc(sizeof(ast_node));
	strncpy(ret->type, type, 32);
	ret->child1 = child1;
	ret->child2 = child2;
	return ret;
}

static PyObject *get_type(PyObject *self, PyObject *args)
{
	PyObject *o;
	if (!PyArg_ParseTuple(args, "O", &o)) return NULL;
	ast_node *node = (ast_node *) PyCapsule_GetPointer(o, NULL);
	printf("type: %s\n", node->type);
	return Py_BuildValue("s", node->type);
}

static PyObject *get_left_child(PyObject *self, PyObject *args)
{
	PyObject *o;
	if (!PyArg_ParseTuple(args, "O", &o)) return NULL;
	ast_node *node = (ast_node *) PyCapsule_GetPointer(o, NULL);
	return PyCapsule_New((void *) node->child1, NULL, NULL);
}

static PyObject *get_right_child(PyObject *self, PyObject *args)
{
	PyObject *o;
	if (!PyArg_ParseTuple(args, "O", &o)) return NULL;
	ast_node *node = (ast_node *) PyCapsule_GetPointer(o, NULL);
	return PyCapsule_New((void *) node->child2, NULL, NULL);
}

static PyMethodDef node_methods[] = {
	{"get_type", get_type, METH_VARARGS, "Get type of node."},
	{"get_left_child", get_left_child, METH_VARARGS, "Get left child of node."},
	{"get_right_child", get_right_child, METH_VARARGS, "Get right child of node."},
	{NULL, NULL, 0, NULL}
};

static PyModuleDef node_module = {
	PyModuleDef_HEAD_INIT,
	"ast_node",
	"docstring",
	-1,
	node_methods
};

PyMODINIT_FUNC PyInit_ast_node()
{
	PyObject *m = PyModule_Create(&node_module);
	if (m == NULL) return NULL;

	return m;
}
