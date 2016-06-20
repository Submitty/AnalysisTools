import java.util.HashMap;
import java.util.Stack;

import org.eclipse.jdt.core.dom.*;
/*
 * Must be run on a Class or Interface node, 
 * or a collection thereof. Can be run
 * on sequential trees wihtout losing data, 
 * 
 */
public class UMLvisitor extends ASTVisitor {

	private HashMap<String, ClassData> data;
	private Stack<String> class_names;
	public HashMap<String, ClassData> getData()
	{
		return data;
	}
	public UMLvisitor()
	{
		data = new HashMap<String, ClassData>();
		class_names = new Stack<String>();
	}
	@Override
	public boolean visit(FieldDeclaration node)
	{
		addUsedType(node.getType());
		return false;
	}
	@Override
	public boolean visit(ClassInstanceCreation node)
	{
		addUsedType(node.getType());
		return true;
	}
	@Override
	public boolean visit(SingleVariableDeclaration node)
	{
		addUsedType(node.getType());
		return false;
	}	
	@Override
	public boolean visit(MethodDeclaration node)
	{	
		addUsedType(node.getReturnType2());
		return true;
	}
	@Override
	public boolean visit(ThrowStatement node)
	{
		ClassInstanceCreation c = (ClassInstanceCreation) node.getExpression();
		addUsedType(c.getType());
		return false;
	}

	/*
	* When encountering a Type Declaration, add the type to 
	* a stack and gather the data. when done with the typeDeclaration's
	* children, remove name from stack, as it is done. 
	* Works on nested types.
	*/
	@Override
	public boolean visit(TypeDeclaration node)
	{
		data.put(node.getName().toString(), new ClassData(node));
		class_names.push(node.getName().toString());
		return true;
	}
	@Override
	public void endVisit(TypeDeclaration node)
	{
		class_names.pop();
	}
	private void addUsedType(Type t)
	{
		data.get(class_names.peek()).addUseType(t);
	}
}
