from distutils.core import setup, Extension
setup(
        name="node",
        version="0.0",
        ext_modules=[
            Extension("ast_node", ["lang/ast_node.c"]),
            Extension("c", ["lang/c/pymod.c", "lang/ast_node.c", "lang/c/parse.out.c", "lang/c/lex.out.c"])])
