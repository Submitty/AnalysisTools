//Use to debug 
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;

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
	 */
	private final static String[] files = new String[]{"./csci2600/src/hw9/FindPathListener.java"
		,"./csci2600/src/hw9/MapPanel.java"
		,"./csci2600/src/hw9/Pathfinder.java"
		,"./csci2600/src/hw9/RPICampusPathsMain.java"};
	
	public static void main(String[] args) {
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
		UMLtest(asts);
		//TreeTest(asts);
	}
	public static ASTParser getParser()
	{
		ASTParser parser = ASTParser.newParser(AST.JLS4);
		parser.setKind(ASTParser.K_COMPILATION_UNIT);
		parser.setResolveBindings(true);
		return parser;
	}
	
	public static void TreeTest(ASTNode[] asts)
	{
		TreeMakerVisitor v = new TreeMakerVisitor();
		asts[0].accept(v);
		System.out.println(v.getTree());
	}
	
	public static void UMLtest(ASTNode[] asts)
	{
		UMLvisitor v = new UMLvisitor();
		for (int i = 0; i < asts.length; i++)
		{
			asts[i].accept(v);
		}
		//print data
		HashMap<String,ClassData> data = v.getData();
		for (ClassData c : data.values())
		{
			c.clean();
			System.out.println(c.toString());
		}
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
