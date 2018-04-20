import ast 
import sys
file =  sys.argv[1]

buffer = open(file).read()
f = open('out.txt', 'w')
tree = ast.parse(buffer)
#exec(compile(tree, "<ast>", "exec"))



class Visitor(ast.NodeVisitor):
	def nestedIfHelper(self, node, level):
		output = "\n<ifStatement," + str(level) + ">\n"
		f.write(output)
		output = ""
		self.generic_visit(node.test, level+1, node)
		output += "</cond,1>\n"
		output += "<compoundStmt,"+ str(level+1) + ">\n"
		f.write(output)
		output = ""
		for child in node.body:
				self.generic_visit(child, level+2, node)
		
		if len(node.orelse) == 1 and isinstance(node.orelse[0], ast.If):
			self.nestedIfHelper(node.orelse[0], level)
		elif len(node.orelse) > 1 or (len(node.orelse) > 0 and not isinstance(node.orelse[0], ast.If)):
			output = "<elseStatement," + str(level) + ">\n"

			for orelse in node.orelse:
				f.write(output)
				output = ""
				self.generic_visit(orelse, level+1, node)


	
	'''
	def chainedCalls(self, node, output, level, prevLevel):
		strlevel = str(level)
		strPrevLevel = str(prevLevel)
		if strPrevLevel  == 0 or strlevel == 0: prevLevel = 1; level = 2; strPrevLevel = 1; strLevel = 2
		if(isinstance(node, ast.Call)):
			if(hasattr(node.func, "value") and not hasattr(node.func.value, "id")):
				output += "\n<calling func: "
				output += node.func.attr
				output += "," + strlevel +  ">"
				output += "\n<args, " + str(level+1) + ">\n"
				f.write(output);
				output = ""
				for arg in node.args:
					self.generic_visit(arg, 2)
					f.write(output)
					output = ""
				output += "</args,1>\n"

				f.write(output);
				output = ""

				self.chainedCalls(node.func.value, output, level+1, level)				
				return

			elif(hasattr(node.func, "value")):
				output += "\n<object: "
				output += node.func.value.id
				output += "; calling func: "
				output += node.func.attr
				output += "," + strPrevLevel +  ">"
				f.write(output);
				output = ""
				self.chainedCalls(node.func, output, level+1, level)				
			else:
				output += "\n<calling func: "
				output += node.func.id
				output += "," + strlevel + ">"
				f.write(output)
				output = ""
				self.chainedCalls(node.func, output, level+1, level)

			output += "\n<args, " + strlevel + ">\n"
			f.write(output);
			output = ""
			for arg in node.args:
				self.generic_visit(arg, 2)
				f.write(output)
				output = ""
			output += "</args,1>\n"
			f.write(output);
			output = ""
		else:
			return output

	'''

	def visit(self, node):
		self.generic_visit(node)

	#each time a node is visited, this helper function is called
	def generic_visit(self, node, level=1, parent=None):
		output = ""
		#variables used for searching children
		hasBody = False
		hasChildren = False	
		hasLeft = False
		hasRight = False
		hasElts = False
		hasExcept = False
		hasFunc = False
		hasTargets = False
		hasValues = False
		hasArgs = False
		hasSlice = False
		hasComparators = False
		hasValue = False
		#calculate the nextLevels
		nextLevel = level+1
		nextNextLevel = nextLevel+1
		strlevel = str(level)
		#converte the next levels to strings
		strNextLevel = str(nextLevel)
		strNextNextLevel = str(nextNextLevel)
		prevLevel = level-1
		strPrevLevel = str(prevLevel)
		
		if isinstance(node, ast.Module):
			output += "<module," + strlevel + ">"
			hasChildren = True
		elif isinstance(node, ast.FunctionDef):
			output += "<functionDef," + strlevel + ">"
			output += "\n<name: " + node.name + "," + strNextLevel + ">"
			output += "\n<compoundStmt," + strNextLevel + ">"
			hasBody = True
		elif isinstance(node, ast.ClassDef):
			output += "<classDef," + strlevel + ">"
			output += "\n<name: " + node.name  + "," + strNextLevel + ">"
			output += "\n<bases," + strNextLevel + ">"
			for base in node.bases:
				output += "\n<base: " + base.id + "," + strNextNextLevel +  ">"
			hasChildren = True
		elif isinstance(node, ast.Index):
			hasValue = True
		elif isinstance(node, ast.Subscript):
			output += "<subscript," + strlevel + ">"
			hasValue = True
			hasSlice = True
		elif isinstance(node, ast.Return):
			output += "<return," + strlevel + ">"
			hasValue = True
		elif isinstance(node, ast.Assign):
			output += "<assignment," + strlevel + ">"
			hasValue = True
			hasTargets = True
		elif isinstance(node, ast.AugAssign):
			output += "<augAssign,"+ strlevel + ">"
			hasValue = True
		elif isinstance(node, ast.For):
			output += "<forLoop," + strlevel + ">\n"
			f.write(output)
			output = ""
			self.generic_visit(node.iter, nextLevel, node)
			output += "\n<compoundStmt," + strNextLevel + ">"
			hasBody = True
		elif isinstance(node, ast.While):
			hasBody = True
			output += "<whileLoop," + strlevel + ">\n"
			f.write(output)
			output = ""
			self.generic_visit(node.test, nextLevel, node)
			output += "<compoundStmt," + strNextLevel + ">"
		elif isinstance(node, ast.If):
			#dummy adl str node
			output += "<ifBlock," + strlevel + ">\n"
			f.write(output)
			output = ""

			self.nestedIfHelper(node, level+1)
		elif isinstance(node, ast.Raise):
			output += "<raisingException," + strlevel + ">"
		elif isinstance(node, ast.Try):
			output += "<try," + strlevel + ">"
			hasExcept = True
			hasBody = True
			output += "\n<compoundStmt," + strNextLevel + ">"
		elif isinstance(node, ast.ExceptHandler):
			hasBody = True
		elif isinstance(node, ast.Import) or isinstance(node, ast.ImportFrom):
			output +=  "<importing," + strlevel + ">"
			for alias in node.names:
				output += "\n<name: " + alias.name + "," +  strNextLevel + ">"
			output += "\n</importing,1>"
		elif isinstance(node, ast.BoolOp):
			output += "<binaryOp," + strlevel + ">"
			hasValues = True
		elif isinstance(node, ast.BinOp):
			output += "<binaryOp," + strlevel + ">"
			hasLeft = True
			hasRight = True
		elif isinstance(node, ast.UnaryOp):
			output += "<unaryOp," + strlevel + ">"
		elif isinstance(node, ast.Compare):
			output += "<comparison," + strlevel + ">"
			hasLeft = True
			hasComparators = True
		elif isinstance(node, ast.Call):
			hasArgs = True
			hasFunc = True
			if isinstance(node.func, ast.Attribute):
				#calling from an object
				'''
				if(isinstance(node.func.value, ast.Call)):
					self.chainedCalls(node,output,level, prevLevel)
				else:'''
				if True:
					if(hasattr(node.func.value, "id")):
						output += "<object: "
						output += node.func.value.id
						output += "; calling func: "
						output += node.func.attr
						output += "," + strlevel +  ">"
						#output += "," + strPrevLevel +  ">"

					else:
						output += "<calling func: "
						output += node.func.attr
						output += "," + strlevel +  ">"
						#output += "," + strPrevLevel +  ">"


				'''
					output += "\n<args, " + strNextLevel + ">\n"
					f.write(output);
					output = ""
					for arg in node.args:
						self.generic_visit(arg, 2)
						f.write(output);
						output = ""
					output += "</args,1>\n"
				'''

			elif isinstance(node.func, ast.Name):
				output += "<calling func: "
				output += node.func.id
				if node.func.id == "print": 
					output += "," + strlevel +  ">"
					#output += "," + strPrevLevel +  ">"
				else: output += "," + strlevel +  ">"
	
				'''

				output += "\n<args, " + strNextLevel + ">\n"
				f.write(output);
				output = ""
				for arg in node.args:
					self.generic_visit(arg, 2)
					f.write(output);
					output = ""
				output += "</args,1>\n"
				'''
		elif isinstance(node, ast.Expr) or isinstance(node, ast.Attribute):
			self.generic_visit(node.value, level, node)
		elif isinstance(node, ast.List):
			output += "<list," + strlevel + ">"
			hasElts = True
		elif isinstance(node, ast.Tuple):
			output += "<tuple," + strlevel + ">"
			hasElts = True
		elif isinstance(node, ast.Dict):
			output += "<dict," + strlevel + ">"
			hasChildren = True
		elif isinstance(node, ast.Set):
			output += "<set," + strlevel + ">"
			hasElts = True

		
		#if (isinstance(node, ast.FunctionDef) or hasBody or hasattr(node, "orelse") or hasExcept) and len(output) != 0:
		
		if (hasBody or hasExcept) and len(output) != 0:
			nextLevel += 1

		if len(output) != 0:
			output += "\n"
			f.write(output)
			output = ""

		if hasValue:
			#self.generic_visit(node.value, nextLevel+1, node)
			self.generic_visit(node.value, nextLevel, node)

		if hasSlice:
			self.generic_visit(node.slice, nextLevel+1, node)

		if hasTargets:
			for target in node.targets:
				self.generic_visit(target, nextLevel, node)

		if hasValues:
			count = 1
			temp = 1
			numValuesLeft = len(node.values)
			
			for value in reversed(node.values):
				if count % 2 == 0 and numValuesLeft > 1:
					output += "<binaryOp," + str(nextLevel+temp) + ">"
					count += 1
					temp+=1

				if len(output) != 0:
					output += "\n"
					f.write(output)
					output = ""

				self.generic_visit(value, nextLevel+temp, node)
				numValuesLeft -= 1
				count += 1

		if hasComparators:
			for comp in node.comparators:
				self.generic_visit(comp, nextLevel, node)

		if hasArgs:
			output += "<args, " + strNextLevel + ">\n"
			f.write(output);
			output = ""
			for arg in node.args:
				self.generic_visit(arg, nextLevel+1, node)
				f.write(output);
				output = ""
			output += "</args,1>\n"
			f.write(output);
			output = ""

		if hasFunc:
			self.generic_visit(node.func, nextLevel, node)
		
		if hasLeft:
			self.generic_visit(node.left, nextLevel, node)
		
		if hasRight:	
			self.generic_visit(node.right, nextLevel, node)

		if hasElts: 
			for elmnt in node.elts:
				self.generic_visit(elmnt, nextLevel, node)

		if hasChildren:
			for child in ast.iter_child_nodes(node):
				self.generic_visit(child, nextLevel, node)
		if hasBody:
		#elif hasattr(node, "body"):
			for child in node.body:
				self.generic_visit(child, nextLevel, node)


		'''	
		if(hasattr(node, "orelse") and len(node.orelse) > 0):
			orelse = node.orelse[0]
			if(not isinstance(orelse, ast.If)):
				output = "<elseStatement," + strNextLevel + ">\n"
				f.write(output)
				for bodynode in node.orelse:
					self.generic_visit(bodynode, nextLevel+1, node)
			else:
				self.generic_visit(orelse, level, node)
		'''

		if hasExcept:
			if len(node.handlers) > 0:
				output = "<except," + strNextLevel + ">\n"
				output += "<compoundStmt," + strNextNextLevel + ">\n"
				f.write(output)

			for handler in node.handlers:
				self.generic_visit(handler, nextLevel, node)

Visitor().visit(tree)
