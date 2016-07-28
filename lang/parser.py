"""Parser interface.

This module exposes an interface to the various language parsers.
"""

import os
import subprocess
import pickle

from  functools import reduce

import lang.python.conversion

MOD_PATH = os.path.dirname(os.path.abspath(__file__))

def walk(node):
    """
    Perform a preorder traversal on tree `node`.
    """
    yield node
    for gen in [walk(c) for c in node.children if c]:
        for elem in gen:
            yield elem

def free_vars(root):
    """
    Obtain a list of the free variables of a node.
    """
    if root.name == "module" or root.name == "interactive" or root.name == "suite":
        raise RuntimeError("Attempted to calculate free variables of block")
    elif root.name == "name":
        return set(root.data)
    elif root.name == "for":
        list_child_sets = [free_vars(x) for x in root.children[1:]]
        child_set = reduce(lambda x, y: x.union(y), list_child_sets, set())
        return child_set.difference(free_vars(root.children[0]))
    else:
        return reduce(lambda x, y: free_vars(x).union(free_vars(y)), root.children, set())

globals()["python"] = lang.python.conversion.python

def python2(data):
    parser = subprocess.Popen([os.path.join(MOD_PATH, "python2", "parse")],
                              stdin=subprocess.PIPE,
                              stdout=subprocess.PIPE)
    parser.stdin.write(bytes(data, "UTF-8"))
    parser.stdin.close()
    parser.wait()
    return pickle.load(parser.stdout)
