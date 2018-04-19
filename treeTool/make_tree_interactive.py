import sys

file=open("/usr/local/submitty/SubmittyAnalysisTools/treeTemplate1.txt");
#file=open("treeTemplate1.txt");

output=file.read();
file=open(sys.argv[1]);
output+="\n\t\t\tvar file="+file.read()+";\n";

file=open("/usr/local/submitty/SubmittyAnalysisTools/treeTemplate2.txt");
#file=open("treeTemplate2.txt");
output+=file.read();
print(output);
