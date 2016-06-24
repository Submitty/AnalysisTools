//Use to debug 
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.eclipse.jdt.core.dom.*;



public class Loader {
	/*private final static String[] files = new String[]{"./csci2600/src/hw0/Ball.java", 
	                                       "./csci2600/src/hw0/BallComparator.java",
	                                       "./csci2600/src/hw0/BallContainer.java",
	                                       "./csci2600/src/hw0/Box.java"};
	*/
	/*private final static String[] files = new String[]{"./csci2600/src/hw8/AndExp.java",
		"./csci2600/src/hw8/BooleanExp.java", "./csci2600/src/hw8/CompositeExp.java","./csci2600/src/hw8/ConstExp.java",
		"./csci2600/src/hw8/EvaluateVisitor.java","./csci2600/src/hw8/ExpressionParser.java","./csci2600/src/hw8/MidScriptVisitor.java",
		"./csci2600/src/hw8/NotExp.java","./csci2600/src/hw8/OrExp.java","./csci2600/src/hw8/PreScriptVisitor.java",
		"./csci2600/src/hw8/VarExp.java","./csci2600/src/hw8/Visitor.java"
	};
	*/
	/*private final static String[] files = new String[]{"./csci2600/src/hw4/Graph.java", 
		"./csci2600/src/hw4/IntPriorityQueue.java","./csci2600/src/hw4/GraphWrapper.java"
	};
	 
	private final static String[] files = new String[]{"./csci2600/src/hw9/FindPathListener.java"
		,"./csci2600/src/hw9/MapPanel.java"
		,"./csci2600/src/hw9/Pathfinder.java"
		,"./csci2600/src/hw9/RPICampusPathsMain.java"};
	*/
	public static void main(String[] args) throws FileNotFoundException, UnsupportedEncodingException {
		String[] files = new String[args.length-3];
		String out = "";
		int ind = 0;
		int mode = -1; //0 = Hierarchy, 1 = AST
		for (int i = 0; i < args.length;i++)
		{

			if (args[i].substring(0,1).equals("-"))
			{
				if (args[i].equals("-u") )
					mode = 0;
				else if(args[i].equals("-t"))
					mode = 1;
				else if (args[i].equals("-o"))
				{
					out = args[i+1];
					i = i + 1;
				}
				else
				{
					System.out.println("invalid flag");
					return;
				}
			}
			else
				files[ind++] = args[i];
		}
		if (out.length() == 0)
		{
			System.out.println("no output specified");
			return;
		}
		//get and set up parser
		ASTParser parser= getParser();
		//set up AST array
		ASTNode[] asts = new ASTNode[files.length];
		//read files and parse to ASTs
		for (int i = 0; i < files.length; i++)
		{
			try
			{
				parser.setSource(ReadFileToCharArray(files[i]));	
			} 
			catch (Exception e)	{e.printStackTrace();}
			asts[i] = parser.createAST(null);
		}
		PrintWriter writer = new PrintWriter(out, "UTF-8");

		if (mode == 0)
		{
			Map<String, ClassData>  data = getHierarchicalData(asts);
			for (String name: data.keySet())
			{
				writer.println(name);
			}
			for (ClassData c: data.values())
			{
				writer.println(c.getName() + " isa  " + c.getClass_extends());
				for (String s: c.getClass_implements())
					writer.println(c.getName() + " isa " + s);
				for (String s: c.getTypesUsed())
					writer.println(c.getName() + " hasa " + s);
			}
		}
		else if (mode == 1)
		{
			writer.print(XMLTree(asts));
		}
		writer.close();
	}
	public static ASTParser getParser()
	{
		ASTParser parser = ASTParser.newParser(AST.JLS4);
		parser.setKind(ASTParser.K_COMPILATION_UNIT);
		parser.setResolveBindings(true);
		return parser;
	}
	
	public static String XMLTree(ASTNode[] asts)
	{
		TreeMakerVisitor v = new TreeMakerVisitor();
		for (ASTNode ast: asts)
			ast.accept(v);
		return "<project>" + v.getTree() + "</project>";
	}
	
	public static Map<String, ClassData> getHierarchicalData(ASTNode[] asts)
	{
		HierarchyVisitor v = new HierarchyVisitor();
		for (int i = 0; i < asts.length; i++)
		{
			asts[i].accept(v);
		}
		//print data
		HashMap<String,ClassData> data = v.getData();
		for (ClassData c : data.values())
		{
			c.clean();
		}
		return data;
	}
	public static char[] ReadFileToCharArray(String filePath) throws IOException {
		StringBuilder fileData = new StringBuilder(1000);
		BufferedReader reader = new BufferedReader(new FileReader(filePath));
 
		char[] buf = new char[10];
		int numRead = 0;
		while ((numRead = reader.read(buf)) != -1) {
			String readData = String.valueOf(buf, 0, numRead);
			fileData.append(readData);
			buf = new char[1024];
		}
 
		reader.close();
 
		return  fileData.toString().toCharArray();	
	}

	

}
