from graph_tool.all import *
import sys

#file format:
#list of nodes
##isa and hasa relations in the forms:
#nodeA isa nodeB
#nodeD hasa nodeE
#the isa node is connected to a node with a loopback and connection to the hasa graph
#returns ({node name : [isa graph vertex refrence, inter vertex hasa graph vertex refrence]}, Graph)
def readFile(readfile):
	inFile = open(readfile, 'r')
	graph = Graph()
	vertexes = {}
	for line in inFile:
		tokens = line.split()
		if len(tokens) == 1:
			vertexes[tokens[0]] = [graph.add_vertex(),graph.add_vertex(),graph.add_vertex()]
			graph.add_edge(vertexes[tokens[0]][0],vertexes[tokens[0]][1])
			graph.add_edge(vertexes[tokens[0]][1],vertexes[tokens[0]][1])
			graph.add_edge(vertexes[tokens[0]][1],vertexes[tokens[0]][2])
		elif len(tokens) == 3:
			if tokens[0] in vertexes and tokens[2] in vertexes:
				if tokens[1] == 'isa':
					graph.add_edge(vertexes[tokens[0]][0], vertexes[tokens[2]][0])
				elif tokens[1] == 'hasa':
					graph.add_edge(vertexes[tokens[0]][2], vertexes[tokens[2]][2])
	
	inFile.close()
	return (vertexes, graph)

##

student_graph = readFile(sys.argv[1])
sub_graph = readFile(sys.argv[2])
sub_iso = subgraph_isomorphism(sub_graph[1], sub_graph[1])
sub_student = subgraph_isomorphism(sub_graph[1], student_graph[1])

print "subiso: " + str(len(sub_iso))
print "student iso: " + str(len(sub_student))
