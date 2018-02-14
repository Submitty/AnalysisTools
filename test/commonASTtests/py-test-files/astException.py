import ast
buffer = open("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/py-test-files/exception.py").read()
tree = ast.parse(buffer)
exec(compile(tree, "<ast>", "exec"))


def searchSubTree(node):
	if isinstance(node, ast.Raise):
		print "an exception could possibly be raised"
	elif isinstance(node, ast.If or ast.While or ast.For):
		for child in node.body:
			searchSubTree(child)

exceptionPresent = False
tryPresent = False

for node in ast.walk(tree):
	if isinstance(node, ast.FunctionDef):
		print "in func"
		for child in node.body:
			searchSubTree(child)
	elif isinstance(node, ast.TryExcept):
		print "handling the exception"


