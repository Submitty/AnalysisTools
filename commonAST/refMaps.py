import context
import eqTag

'''
Hex values for colors of marked nodes
'''

adlDetailColor = "#B0B0B0"
adlStrColor = "#8b98ca"
notMatchedColor = "#ff0000"

'''
The tagEqlMap is a dictionary of nodes in the common AST, to their equivalent nodes in the full AST. 
This map is only used when the tags are not (case in-sensitive) equivalent. 
For example, in python, "For" in the common AST matches "for" in the full AST. 
Because the only differences are case, these constructs do NOT need to be added to this map.
However, in python, "FunctionDef" in the common AST matches to "function" in the full AST. So, it must be added to the tagEqlMap.

The structure of the tagEqlMap is as follows:
	commonASTConstruct: [EqualityTagObj1, EqualityTagObj2, ..., EqualityTagObjN]

where EqualityObjs are instances of the EqTag class in eqTag.py 
Equality Objects contain a list of tags that must all be in the potential match, 
                         a context that the match must be in (children, parents, grand parents etc.)
			 and a language of the full AST


The Context objects are instances of the Context class in context.py
A Context Object contains a list of lookahead tags (children)
	            	  a list of sibling tags 
			  a list of parent tags
 			  and a list of grand parent tags

If the tags, context, and language match, the then common AST node matches to that full AST node.
'''


emptyCntxt = context.Context("py")
emptyCntxtCpp = context.Context("cpp")
classNoCntxt = eqTag.EqTag(["class"], emptyCntxt)
functionNoCntxt = eqTag.EqTag(["function"], emptyCntxt)
caseNoCntxt = eqTag.EqTag(["case"], emptyCntxt)
ifNoCntxt = eqTag.EqTag(["if"], emptyCntxt)
functionNoCntxtCpp = eqTag.EqTag(["function"], emptyCntxtCpp)
augAssignNoCntxt = eqTag.EqTag(["augmented", "assign"], emptyCntxt) 
binOpNoCntxt = eqTag.EqTag(["binary", "operator"], emptyCntxt) 
binOpNoCntxtCpp = eqTag.EqTag(["binaryop"], emptyCntxtCpp) 
unOpNoCntxt = eqTag.EqTag(["unary", "operator"], emptyCntxt) 
listNoCntxt = eqTag.EqTag(["list"], emptyCntxt) 
setNoCntxt = eqTag.EqTag(["set"], emptyCntxt) 
dictNoCntxt = eqTag.EqTag(["dict"], emptyCntxt) 
tupleNoCntxt = eqTag.EqTag(["tuple"], emptyCntxt) 
subscriptNoCntxt = eqTag.EqTag(["subscript"], emptyCntxt)
basesCntxt = eqTag.EqTag(["variable"], context.Context("py", ["\*"],["\*"],["argument"],["parameters"]))
paramClassContext = eqTag.EqTag(["parameters"], context.Context("py", ["\*"],["\*"],["class"],["\*"]))
dottedNameImportCntxt = eqTag.EqTag(["dottedname"], context.Context("py", ["\*"],["\*"],["import"],["\*"]))
dottedNameGpImportCntxt = eqTag.EqTag(["dottedname"], context.Context("py", ["\*"],["\*"],["\*"],["import"]))
bodyIfCntxt = eqTag.EqTag(["body"], context.Context("py",["\*"],["\*"],["case"],["if"]))
bodyForCntxt = eqTag.EqTag(["body"], context.Context("py",["\*"],["\*"],["for"],["\*"]))
bodyWhileCntxt = eqTag.EqTag(["body"], context.Context("py",["\*"],["\*"],["while"], ["\*"]))
bodyFuncCntxt = eqTag.EqTag(["body"], context.Context("py",["\*"],["\*"],["function"],["\*"]))
elseIfCntxt = eqTag.EqTag(["else"], context.Context("py",["\*"],["\*"],["if"],["\*"]))
lteBinOpCntxt = eqTag.EqTag(["binary", "operator"], context.Context("py",["lte"],["\*"], ["\*"],["\*"]))
ltBinOpCntxt = eqTag.EqTag(["binary", "operator"], context.Context("py",["lessthan"],["\*"], ["\*"],["\*"]))
gtBinOpCntxt = eqTag.EqTag(["binary", "operator"], context.Context("py",["gt"],["\*"], ["\*"],["\*"]))
gteBinOpCntxt = eqTag.EqTag(["binary", "operator"], context.Context("py",["gte"],["\*"], ["\*"],["\*"]))
eqBinOpCntxt = eqTag.EqTag(["binary", "operator"], context.Context("py",["equals"],["\*"], ["\*"],["\*"]))
caseIfCntxt = eqTag.EqTag(["case"], context.Context("py",["\*"], ["\*"], ["if"],["\*"]))
ifCppCntxt = eqTag.EqTag(["ifstatement"], context.Context("cpp",["\*"], ["\*"], ["\*"],["\*"]))
forLoopCppCntxt = eqTag.EqTag(["forloop"], context.Context("cpp",["\*"],["\*"],["\*"],["\*"]))
assignCppCntxt = eqTag.EqTag(["assignment"], context.Context("cpp",["\*"],["\*"],["\*"],["\*"]))
whileCppCntxt = eqTag.EqTag(["whileloop"], context.Context("cpp",["\*"],["\*"],["\*"],["\*"]))


tagEqlMap = dict({"classdef": [classNoCntxt], #classdef matches to class in any context
			"functiondef": [functionNoCntxt, functionNoCntxtCpp], 
			"compoundstmt": [bodyIfCntxt, bodyForCntxt, bodyWhileCntxt, bodyFuncCntxt, elseIfCntxt],
			"augassign": [augAssignNoCntxt],
			"binop": [binOpNoCntxt, binOpNoCntxtCpp],
			"unaryop": [unOpNoCntxt],
			"comparison": [eqBinOpCntxt, gtBinOpCntxt, gteBinOpCntxt, ltBinOpCntxt, lteBinOpCntxt],
			"for": [forLoopCppCntxt],
			"while": [whileCppCntxt],
			"assign": [assignCppCntxt],
			"identifier": [dottedNameImportCntxt, dottedNameGpImportCntxt, basesCntxt],
			"bases": [paramClassContext],
			"container": [listNoCntxt, setNoCntxt, dictNoCntxt, tupleNoCntxt],
			"if": [caseNoCntxt, ifCppCntxt],
			"ifblock": [ifNoCntxt, ifCppCntxt],
			"expr": [subscriptNoCntxt]})

'''
The adlDetailMap is a dictionary of nodes in the full AST that are not relevant to our use cases 
and thus do not need to be included in the common AST

The structure of the adlDetail is as follows:
	fullASTConstruct: [Context1, Context2, ..., ContextN]

where Context objects are instances of the Context class in context.py
A Context Object contains a language
			  a list of lookahead tags (children)
	            	  a list of sibling tags 
			  a list of parent tags
 			  and a list of grand parent tags

If the node in the full AST map has a tag equal to a tag in the adlDetailMap, 
and its context matches one of the contexts in its value, the node is additional detail.

Our use cases are as follows:
(1) Detecting nested if statements and dangling elses
(2) Detecting nested loops & crude complexity analysis
(3) Detecting for/while loops and nested if/elses inside 
(4) Detecting member function calls of an outside class
(5) Counting number of while and if statements
(6) Count number of function calls
(7) Exception Handling - make sure exceptions are never thrown
(8) Detecting class heirarchies
(9) Detecting function calls of a forbidden module
(10) Forbidding exec

If you modify the use cases, adjust the adlDetailMap to match your use cases.
'''

assignContext =  context.Context("py",["\*"],["\*"],["assign"],["\*"])
functionContext =  context.Context("py",["\*"],["\*"],["function"],["\*"])
paramContext =  context.Context("py",["\*"],["\*"],["parameters"],["\*"])
accessContext =  context.Context("py",["\*"],["\*"],["access"],["\*"])
augAssignContext =  context.Context("py",["\*"],["\*"],["augmented", "assign"],["\*"])
forContext =  context.Context("py",["\*"],["\*"],["for"],["\*"])
binOpContext = context.Context("py",["\*"], ["\*"],["binary", "operator"], ["\*"])
unOpContext = context.Context("py",["\*"], ["\*"],["unary", "operator"], ["\*"])
adlDetailMap = dict({"literal": [emptyCntxt],
			"parmvar": [emptyCntxtCpp],
			"string": [assignContext, binOpContext],
			"greaterthan": [binOpContext],
			"gt": [binOpContext],
			"gte": [binOpContext],
			"lessthan": [binOpContext],
			"lte": [binOpContext],
			"eq": [binOpContext],
			"and": [binOpContext],
			"or": [binOpContext],
			"string": [emptyCntxt],
			"none": [emptyCntxt],
			"null": [emptyCntxt],
			"variable": [emptyCntxt],
			"parameters": [functionContext],
			"parameter": [paramContext],
			"targets": [forContext],
			"minus": [binOpContext],
			"modulo": [binOpContext],
			"plus": [binOpContext],
			"plusassign": [augAssignContext],
			"divassign": [augAssignContext],
			"floordivassign":[augAssignContext],
			"multassign": [augAssignContext],
			"minusassign": [augAssignContext],
			"exponent": [binOpContext],
			"multiply":[binOpContext],
			"divide":[binOpContext],
			"not":[unOpContext],
			"integerliteral": [emptyCntxtCpp]})


'''
The adlStructMap is a dictionary of nodes in either AST, that if removed, would not affect the structure or data in the AST

The structure of the adlStructMap is as follows:
	languageConstruct: [ContextObj1, ContextObj2, ..., ContextObjN]

Where Context objects are instances of the Context class in context.py
A Context Object contains a list of lookahead tags (children)
	            	  a list of sibling tags 
			  a list of parent tags
 			  and a list of grand parent tags

If the tag and one of the contexts match, the node is marked as an additional structure node
'''

#TODO: Fix this - some of these are language py but they should apply to the common AST
classContext = context.Context("py",["\*"],["\*"],["class"],["\*"])
basesContext = context.Context("py",["\*"],["\*"],["bases"],["\*"]) #commonAST not PY
paramCntxt = context.Context("py",["\*"],["\*"],["parameters"],["class"]) 
importCntxt = context.Context("py",["\*"],["\*"],["import"],["\*"])
importCntxtGP = context.Context("py",["\*"],["\*"],["\*"],["import"])
ifElseContext = context.Context("py",["+case"], ["\*"], ["\*"], ["\*"]) #+ signifys more than one
callContext = context.Context("py",["\*"],["\*"],["call"],["\*"])
functionContext = context.Context("py",["\*"],["\*"],["functiondef"],["\*"])
noChildrenContext = context.Context("py",None,["\*"], ["if"], ["\*"])
whileNoChildrenContext = context.Context("py",None,["\*"], ["while"], ["\*"])
forNoChildrenContext = context.Context("py",None,["\*"], ["for"], ["\*"])
adlStructMap = dict({"body":[classContext], 
			"args":[callContext], 
			#"if": [ifElseContext],
			"access": [emptyCntxt],
			"else": [noChildrenContext, whileNoChildrenContext, forNoChildrenContext],
			"argument":[callContext, paramCntxt],
			"paren":[emptyCntxt],
			"import":[importCntxt],
			"importitem":[importCntxt, importCntxtGP],
			"importeverything":[importCntxt],
			"identifier":[functionContext, classContext]})
