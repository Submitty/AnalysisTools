from distutils.core import setup, Extension
setup(
        name="node",
        version="0.0",
        ext_modules=[
            Extension("ast_node", ["lang/ast_node.c"]),
            Extension("newc", ["lang/newc/pymod.c", "lang/ast_node.c", "lang/newc/parse.out.c", "lang/newc/lex.out.c"])])
