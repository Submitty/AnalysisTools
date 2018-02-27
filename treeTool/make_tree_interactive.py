import sys

file=open("treeTemplate1.txt");

output=file.read();
file=open(sys.argv[1]);
output+="\n\t\t\tvar file="+file.read()+";\n";
file=open("treeTemplate2.txt");
output+=file.read();
print(output);