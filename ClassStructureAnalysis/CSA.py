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
    edges = graph.new_edge_property("string")
    vertexes = {}
    vertex_map = graph.new_vertex_property("string")
    for line in inFile:
        tokens = line.split()
        if len(tokens) == 1:
            vertexes[tokens[0]] = graph.add_vertex()
            vertex_map[vertexes[tokens[0]]] = tokens[0]
        elif len(tokens) == 3:
            if tokens[0] in vertexes and tokens[2] in vertexes:
                e = graph.add_edge(vertexes[tokens[0]], vertexes[tokens[2]])
                edges[e] = tokens[1]
    
    inFile.close()
    return (vertexes,edges, graph, vertex_map)

##

student_graph = readFile(sys.argv[1])
sub_graph = readFile(sys.argv[2])
sub_iso = subgraph_isomorphism(sub_graph[2], sub_graph[2], edge_label=(sub_graph[1],sub_graph[1]))
sub_student = subgraph_isomorphism(sub_graph[2], student_graph[2], edge_label=(sub_graph[1], student_graph[1]))

print "number of isomorphic subgraphs: " + str(len(sub_iso))
print "instances of subraph isomorphism: " + str(len(sub_student))

for nodename in sub_graph[0]:
    tmp = nodename + " could be: "
    vertex = sub_graph[0][nodename]
    aset = set()
    for i in range(0, len(sub_student)):
        vertex_index = sub_student[i][vertex]
        if not vertex_index in aset:
            aset.add(vertex_index)
            tmp = tmp +  student_graph[3][student_graph[2].vertex(vertex_index)] + ", "
    tmp = tmp[0:len(tmp) - 2] + "\n"
    print tmp

#vertex_font_size=12, edge_font_size=12,
FontSize = 14 
graph_draw(student_graph[2],vertex_font_size=FontSize,
	 edge_font_size=FontSize, edge_text=student_graph[1], 
	vertex_text=student_graph[3], output="student_graph.png",
	edge_color=[0,0,0,1], vertex_fill_color=[0.50, 0.50, 0.50, 1.0])

graph_draw(sub_graph[2],vertex_font_size=FontSize,
	 edge_font_size=FontSize,  edge_text=sub_graph[1], 
	vertex_text=sub_graph[3], output="sub_graph.png",
	edge_color=[0,0,0,1], vertex_fill_color=[0.50, 0.50, 0.50, 1.0])
