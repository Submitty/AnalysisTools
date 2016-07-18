"""AST Node.

This module defines the class used to represent abstract syntax trees.
"""

from collections import namedtuple

Node = namedtuple("Node", ["name", "children", "data"])
