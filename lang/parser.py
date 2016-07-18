"""Parser interface.

This module exposes an interface to the various language parsers.
"""

import os
import subprocess
import pickle

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

globals()["python"] = lang.python.conversion.python

def python2(data):
    parser = subprocess.Popen([os.path.join(MOD_PATH, "python2", "parse")],
                              stdin=subprocess.PIPE,
                              stdout=subprocess.PIPE)
    parser.stdin.write(bytes(data, "UTF-8"))
    parser.stdin.close()
    parser.wait()
    return pickle.load(parser.stdout)
