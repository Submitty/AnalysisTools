"""Lexer interface.

This module exposes an interface to the lexer executables in subdirectories of
lang/.
"""

import os
import subprocess
# pylint: disable=redefined-builtin
from functools import reduce

MOD_PATH = os.path.dirname(os.path.abspath(__file__))

TOKENS = {
    "python" : [
        "INVALID",
        "FALSE",
        "NONE",
        "TRUE",
        "AND",
        "AS",
        "ASSERT",
        "BREAK",
        "CLASS",
        "CONTINUE",
        "DEF",
        "DEL",
        "ELIF",
        "ELSE",
        "EXCEPT",
        "FINALLY",
        "FOR",
        "FROM",
        "GLOBAL",
        "IF",
        "IMPORT",
        "IN",
        "IS",
        "LAMBDA",
        "NONLOCAL",
        "NOT",
        "OR",
        "PASS",
        "RAISE",
        "RETURN",
        "TRY",
        "WHILE",
        "WITH",
        "YIELD",
        "PLUS",
        "MINUS",
        "ASTERISK",
        "SLASH",
        "DOUBLE_SLASH",
        "PERCENT",
        "DOUBLE_ASTERISK",
        "EQ_OP",
        "NE_OP",
        "LESS_THAN",
        "GREATER_THAN",
        "LE_OP",
        "GE_OP",
        "AMPERSAND",
        "PIPE",
        "TILDE",
        "CARET",
        "LEFT_OP",
        "RIGHT_OP",
        "LEFT_PAREN",
        "RIGHT_PAREN",
        "LEFT_SQUARE",
        "RIGHT_SQUARE",
        "LEFT_CURLY",
        "RIGHT_CURLY",
        "DOT",
        "COMMA",
        "COLON",
        "SEMICOLON",
        "AT",
        "EQUAL",
        "ADD_ASSIGN",
        "SUB_ASSIGN",
        "MUL_ASSIGN",
        "DIV_ASSIGN",
        "INTDIV_ASSIGN",
        "MOD_ASSIGN",
        "POW_ASSIGN",
        "AND_ASSIGN",
        "OR_ASSIGN",
        "XOR_ASSIGN",
        "LEFT_ASSIGN",
        "RIGHT_ASSIGN",
        "IDENTIFIER",
        "INTEGER_LITERAL",
        "FLOAT_LITERAL",
        "IMAGINARY_LITERAL",
        "STRING_LITERAL",
        "BYTES_LITERAL",
        "INDENT",
        "UNINDENT",
        ],
    "c" : [
        "INVALID",
        "AUTO",
        "BREAK",
        "CASE",
        "CHAR",
        "CONST",
        "CONTINUE",
        "DEFAULT",
        "DO",
        "DOUBLE",
        "ELSE",
        "ENUM",
        "EXTERN",
        "FLOAT",
        "FOR",
        "GOTO",
        "IF",
        "INLINE",
        "INT",
        "LONG",
        "REGISTER",
        "RESTRICT",
        "RETURN",
        "SHORT",
        "SIGNED",
        "SIZEOF",
        "STATIC",
        "STRUCT",
        "SWITCH",
        "TYPEDEF",
        "UNION",
        "UNSIGNED",
        "VOID",
        "VOLATILE",
        "WHILE",
        "ALIGNAS",
        "ALIGNOF",
        "ATOMIC",
        "BOOL",
        "COMPLEX",
        "GENERIC",
        "IMAGINARY",
        "NORETURN",
        "STATIC_ASSERT",
        "THREAD_LOCAL",
        "FUNC_NAME",
        "IDENTIFIER",
        "INTEGER_LITERAL",
        "FLOAT_LITERAL",
        "STRING_LITERAL",
        "ELLIPSIS",
        "RIGHT_ASSIGN",
        "LEFT_ASSIGN",
        "ADD_ASSIGN",
        "SUB_ASSIGN",
        "MUL_ASSIGN",
        "DIV_ASSIGN",
        "MOD_ASSIGN",
        "AND_ASSIGN",
        "XOR_ASSIGN",
        "OR_ASSIGN",
        "RIGHT_OP",
        "LEFT_OP",
        "INC_OP",
        "DEC_OP",
        "PTR_OP",
        "AND_OP",
        "OR_OP",
        "LE_OP",
        "GE_OP",
        "EQ_OP",
        "NE_OP",
        "SEMICOLON",
        "LEFT_CURLY",
        "RIGHT_CURLY",
        "COMMA",
        "COLON",
        "EQUAL",
        "LEFT_PAREN",
        "RIGHT_PAREN",
        "LEFT_SQUARE",
        "RIGHT_SQUARE",
        "DOT",
        "AMPERSAND",
        "EXCLAMATION",
        "TILDE",
        "MINUS",
        "PLUS",
        "ASTERISK",
        "SLASH",
        "PERCENT",
        "LESS_THAN",
        "GREATER_THAN",
        "CARET",
        "PIPE",
        "QUESTION"
        ]
}

def can_be_int(string):
    """
    Return true if string can be converted to an integer.
    """
    try:
        int(string)
        return True
    except ValueError:
        return False

def lexer(lang):
    """
    Generate a function that will call the lexer executable for lang.
    """
    def inner(data):
        """
        Function lexing data and returning a list of token names.
        """
        lex = subprocess.Popen([os.path.join(MOD_PATH, lang, "lex")],
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE)
        lex.stdin.write(bytes(data, "UTF-8"))
        lex.stdin.close()
        stripped = [x.decode().strip() for x in lex.stdout.readlines()]
        lines = [x.split(" ") for x in stripped if len(x) > 0]
        token_indices = reduce(lambda x, y: x + y, lines)[0::2]
        filtered = [x for x in token_indices if can_be_int(x)]
        return [TOKENS[lang][int(x)].lower() for x in filtered]
    return inner

for l in TOKENS:
    globals()[l] = lexer(l)
