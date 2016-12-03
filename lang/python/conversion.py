import ast

from lang.node import Node

def python_ast_to_node(tree):
    ttype = type(tree)
    if ttype is ast.Module:
        return Node("module",
                    [python_ast_to_node(n) for n in tree.body],
                    None)
    elif ttype is ast.Interactive:
        return Node("interactive",
                    [python_ast_to_node(n) for n in tree.body],
                    None)
    elif ttype is ast.Expression:
        return Node("expression",
                    python_ast_to_node(tree.body),
                    None)
    elif ttype is ast.Suite:
        return Node("suite",
                    [python_ast_to_node(n) for n in tree.body],
                    None)
    elif ttype is ast.FunctionDef:
        return Node("function_def",
                    [python_ast_to_node(tree.name),
                     python_ast_to_node(tree.args)] +
                    [python_ast_to_node(n) for n in tree.body] +
                    [python_ast_to_node(tree.decorator_list),
                     python_ast_to_node(tree.returns)],
                    None)
    elif ttype is ast.ClassDef:
        return Node("class_def",
                    [python_ast_to_node(tree.name),
                     python_ast_to_node(tree.bases),
                     python_ast_to_node(tree.keywords),
                     python_ast_to_node(tree.starargs),
                     python_ast_to_node(tree.kwargs)] +
                    [python_ast_to_node(n) for n in tree.body] +
                    [python_ast_to_node(tree.decorator_list)],
                    None)
    elif ttype is ast.Return:
        return Node("return",
                    [python_ast_to_node(tree.value)],
                    None)
    elif ttype is ast.Delete:
        return Node("delete",
                    [python_ast_to_node(n) for n in tree.targets],
                    None)
    elif ttype is ast.Assign:
        return Node("assign",
                    [python_ast_to_node(tree.value)] +
                    [python_ast_to_node(n) for n in tree.targets],
                    None)
    elif ttype is ast.AugAssign:
        return Node("aug_assign",
                    [python_ast_to_node(tree.value),
                     python_ast_to_node(tree.target)],
                    python_ast_to_node(tree.op))
    elif ttype is ast.For:
        return Node("for",
                    [python_ast_to_node(tree.target),
                     python_ast_to_node(tree.iter)] +
                    [python_ast_to_node(n) for n in tree.body] +
                    [python_ast_to_node(n) for n in tree.orelse],
                    None)
    elif ttype is ast.While:
        return Node("while",
                    [python_ast_to_node(tree.test)] +
                    [python_ast_to_node(n) for n in tree.body] +
                    [python_ast_to_node(n) for n in tree.orelse],
                    None)
    elif ttype is ast.If:
        return Node("if",
                    [python_ast_to_node(tree.test)] +
                    [python_ast_to_node(n) for n in tree.body] +
                    [python_ast_to_node(n) for n in tree.orelse],
                    None)
    elif ttype is ast.With:
        return Node("with",
                    [python_ast_to_node(n) for n in tree.items] +
                    [python_ast_to_node(n) for n in tree.body],
                    None)
    elif ttype is ast.Raise:
        return Node("raise",
                    [python_ast_to_node(tree.exc),
                     python_ast_to_node(tree.cause)],
                    None)
    elif ttype is ast.Try:
        return Node("try",
                    [python_ast_to_node(n) for n in tree.body] +
                    [python_ast_to_node(n) for n in tree.handlers] +
                    [python_ast_to_node(n) for n in tree.orelse] +
                    [python_ast_to_node(n) for n in tree.finalbody],
                    None)
    elif ttype is ast.Assert:
        return Node("assert",
                    [python_ast_to_node(tree.test),
                     python_ast_to_node(tree.msg)],
                    None)
    elif ttype is ast.Import:
        return Node("import",
                    [python_ast_to_node(n) for n in tree.names],
                    None)
    elif ttype is ast.ImportFrom:
        return Node("import_from",
                    [python_ast_to_node(tree.module),
                     python_ast_to_node(tree.level)] +
                    [python_ast_to_node(n) for n in tree.names],
                    None)
    elif ttype is ast.Global:
        return Node("global", [], tree.names)
    elif ttype is ast.Nonlocal:
        return Node("nonlocal", [], tree.names)
    elif ttype is ast.Expr:
        return Node("expr",
                    [python_ast_to_node(tree.value)],
                    None)
    elif ttype is ast.Pass:
        return Node("pass", [], None)
    elif ttype is ast.Break:
        return Node("break", [], None)
    elif ttype is ast.Continue:
        return Node("continue", [], None)
    elif ttype is ast.BoolOp:
        return Node(python_ast_to_node(tree.op),
                    [python_ast_to_node(n) for n in tree.values],
                    None)
    elif ttype is ast.BinOp:
        return Node(python_ast_to_node(tree.op),
                    [python_ast_to_node(tree.left),
                     python_ast_to_node(tree.right)],
                    None)
    elif ttype is ast.UnaryOp:
        return Node(python_ast_to_node(tree.op),
                    [python_ast_to_node(tree.operand)],
                    None)
    elif ttype is ast.Lambda:
        return Node("lambda",
                    [python_ast_to_node(tree.args),
                     python_ast_to_node(tree.body)],
                    None)
    elif ttype is ast.IfExp:
        return Node("if_expr",
                    [python_ast_to_node(tree.test),
                     python_ast_to_node(tree.body),
                     python_ast_to_node(tree.orelse)],
                    None)
    elif ttype is ast.Dict:
        return Node("dict",
                    [Node("dict_pair", [k, v], None)
                     for (k, v) in zip(map(python_ast_to_node,
                                           tree.keys
                                           if tree.keys else []),
                                       map(python_ast_to_node,
                                           tree.values
                                           if tree.values else []))],
                    None)
    elif ttype is ast.Set:
        return Node("set",
                    [python_ast_to_node(n) for n in tree.elts],
                    None)
    elif ttype is ast.ListComp:
        return Node("list_comp",
                    [python_ast_to_node(tree.elt)] +
                    [python_ast_to_node(n) for n in tree.generators],
                    None)
    elif ttype is ast.SetComp:
        return Node("set_comp",
                    [python_ast_to_node(tree.elt)] +
                    [python_ast_to_node(n) for n in tree.generators],
                    None)
    elif ttype is ast.DictComp:
        return Node("dict_comp",
                    [python_ast_to_node(tree.key),
                     python_ast_to_node(tree.value)] +
                    [python_ast_to_node(n) for n in tree.generators],
                    None)
    elif ttype is ast.GeneratorExp:
        return Node("generator_expr",
                    [python_ast_to_node(tree.elt)] +
                    [python_ast_to_node(n) for n in tree.generators],
                    None)
    elif ttype is ast.Yield:
        return Node("yield",
                    [python_ast_to_node(tree.value)],
                    None)
    elif ttype is ast.YieldFrom:
        return Node("yield_from",
                    [python_ast_to_node(tree.value)],
                    None)
    elif ttype is ast.Compare:
        return Node("compare",
                    [python_ast_to_node(tree.left)] +
                    [python_ast_to_node(n) for n in tree.comparators],
                    python_ast_to_node(tree.ops))
    elif ttype is ast.Call:
        return Node("call",
                    [python_ast_to_node(tree.func)] +
                    [python_ast_to_node(n) for n in tree.args] +
                    [python_ast_to_node(n) for n in tree.keywords] +
                    [python_ast_to_node(tree.starargs),
                     python_ast_to_node(tree.kwargs)],
                    None)
    elif ttype is ast.Num:
        return Node("num", [], tree.n)
    elif ttype is ast.Str:
        return Node("str", [], tree.s)
    elif ttype is ast.Bytes:
        return Node("bytes", [], tree.s)
    elif ttype is ast.NameConstant:
        return Node("name_constant", [], tree.value)
    elif ttype is ast.Ellipsis:
        return Node("ellipsis", [], None)
    elif ttype is ast.Attribute:
        return Node("attribute",
                    [python_ast_to_node(tree.value)],
                    tree.attr)
    elif ttype is ast.Subscript:
        return Node("subscript",
                    [python_ast_to_node(tree.value),
                     python_ast_to_node(tree.slice)],
                    None)
    elif ttype is ast.Starred:
        return Node("starred",
                    [python_ast_to_node(tree.value)],
                    None)
    elif ttype is ast.Name:
        return Node("name", [], tree.id)
    elif ttype is ast.List:
        return Node("list",
                    [python_ast_to_node(n) for n in tree.elts],
                    None)
    elif ttype is ast.Tuple:
        return Node("tuple",
                    [python_ast_to_node(n) for n in tree.elts],
                    None)
    elif ttype is ast.Slice:
        return Node("slice",
                    [python_ast_to_node(tree.lower),
                     python_ast_to_node(tree.upper),
                     python_ast_to_node(tree.step)],
                    None)
    elif ttype is ast.ExtSlice:
        return Node("ext_slice",
                    [python_ast_to_node(n) for n in tree.dims],
                    None)
    elif ttype is ast.Index:
        return Node("index",
                    [python_ast_to_node(tree.value)],
                    None)
    elif ttype is ast.comprehension:
        return Node("comprehension",
                    [python_ast_to_node(tree.target),
                     python_ast_to_node(tree.iter)] +
                    [python_ast_to_node(n) for n in tree.ifs],
                    None)
    elif ttype is ast.ExceptHandler:
        return Node("except_handler",
                    [python_ast_to_node(tree.type)] +
                    [python_ast_to_node(n) for n in tree.body],
                    tree.name)
    elif ttype is ast.arguments:
        return Node("arguments",
                    [python_ast_to_node(n) for n in tree.args] +
                    [python_ast_to_node(tree.vararg)] +
                    [python_ast_to_node(n) for n in tree.defaults],
                    None)
    elif ttype is ast.arg:
        return Node("arg",
                    [python_ast_to_node(tree.annotation)],
                    tree.arg)
    elif ttype is ast.keyword:
        return Node("kwarg",
                    [python_ast_to_node(tree.value)],
                    tree.arg)
    elif ttype is ast.alias:
        return Node("alias", [], (tree.name, tree.asname))
    elif ttype is ast.And:
        return "and"
    elif ttype is ast.Or:
        return "or"
    elif ttype is ast.Add:
        return "add"
    elif ttype is ast.Sub:
        return "sub"
    elif ttype is ast.Mult:
        return "mul"
    elif ttype is ast.Div:
        return "div"
    elif ttype is ast.Mod:
        return "mod"
    elif ttype is ast.Pow:
        return "pow"
    elif ttype is ast.LShift:
        return "left_shift"
    elif ttype is ast.RShift:
        return "right_shift"
    elif ttype is ast.BitOr:
        return "bitwise_or"
    elif ttype is ast.BitXor:
        return "bitwise_xor"
    elif ttype is ast.BitAnd:
        return "bitwise_and"
    elif ttype is ast.FloorDiv:
        return "floor_div"
    elif ttype is ast.Invert:
        return "invert"
    elif ttype is ast.Not:
        return "not"
    elif ttype is ast.UAdd:
        return "positive"
    elif ttype is ast.USub:
        return "negative"
    elif ttype is ast.Eq:
        return "equal"
    elif ttype is ast.NotEq:
        return "not_equal"
    elif ttype is ast.Lt:
        return "less_than"
    elif ttype is ast.LtE:
        return "less_than_equal"
    elif ttype is ast.Gt:
        return "greater_than"
    elif ttype is ast.GtE:
        return "greater_than_equal"
    elif ttype is ast.Is:
        return "is"
    elif ttype is ast.IsNot:
        return "is_not"
    elif ttype is ast.In:
        return "in"
    elif ttype is ast.NotIn:
        return "not_in"

def python(data):
    tree = ast.parse(data)
    return python_ast_to_node(tree)
