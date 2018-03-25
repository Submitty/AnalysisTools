import json
import anytree
import sys

grayCount1 = 0
grayCount2 = 0

def markSubtree(node):
	markNode(node)
	if not hasattr(node, "children"):
		return

	for child in node["children"]:
		markSubtree(node)

def markNode(node):
	if(hasTags(node)):
		node["tags"].append("blue")
	else:
		node["tags"] = ["blue"]

def equalTags(tag, tag_):
	return tag.lower() == tag_.lower()

def tagsMatch(tags1, tags2):
	for tag in tags1:
		for tag_ in tags2:
			if equalTags(tag, tag_ ):
				return True

	return False;

def hasTags(node):
	return "tags" in node 

def datasMatch(node, node2):
	return node["data"] == node2["data"]

def typesMatch(node, node2):
	return node["type"] == node2["type"]

def checkChildren(t1Nodes, t2Nodes):
	global grayCount1
	global grayCount2

	for node in t1Nodes:
		node["matched"] = False
	for node in t2Nodes:
		node["matched"] = False


	for node in t1Nodes:	
		for node2 in t2Nodes:
			if((hasTags(node) and hasTags(node2) and tagsMatch(node["tags"], node2["tags"]) and not node2["matched"]) or 
				(not hasTags(node) and not hasTags(node2) and typesMatch(node, node2) and datasMatch(node, node2))):

				node["matched"] = True
				node2["matched"] = True

				if "children" in node and "children" in node2:
					checkChildren(node["children"], node2["children"])
				break
	for node in t1Nodes:
		if not node["matched"]:
			markNode(node)
			print (node)
			print ("Marked!")
			markSubtree(node)
			#TODO: check node's children to see if t2 is just skipping node
		else:
			grayCount1 += 1
		del node["matched"]	
			
	for node in t2Nodes:
		if not node["matched"]:
			markNode(node)
			print ("Marked!")
			print (node)
			markSubtree(node)
			#TODO: check node's children to see if t2 is just skipping node
		else:
			grayCount2 += 1
		del node["matched"]

def runner():
	if len(sys.argv) < 3:
		print("error: must specify two files")
		exit()

	with open(sys.argv[1], 'r') as f:
    		jsonObj1 = json.load(f)

	with open(sys.argv[2], 'r') as f:
		jsonObj2 = json.load(f)

	if not hasTags(jsonObj1) or not hasTags(jsonObj2):
		print("error: illformatted json doesn't have tags")
	elif not tagsMatch(jsonObj1["tags"], jsonObj2["tags"]):
		print("error: root nodes don't match")
	else:
		checkChildren(jsonObj1["children"], jsonObj2["children"])
		if not grayCount1 == grayCount2:
			print("error: number of gray nodes in each graph is not equal. We have a problem")

		#output to a new file
		index1 = sys.argv[1].find(".")
		index2 = sys.argv[2].find(".")
		with open(sys.argv[1][0:index1] + "Modified.txt", "w+") as fw:
			fw.write(json.dumps(jsonObj1))
		
		with open(sys.argv[2][0:index2] + "Modified.txt", "w+") as fw:
			fw.write(json.dumps(jsonObj2))
runner()
