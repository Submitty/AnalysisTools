import org.eclipse.jdt.core.dom.*;

public class TreeMakerVisitor extends ASTVisitor {

	private String xml;
	public String getTree()
	{
		return xml;
	}
	
	@Override
	public boolean visit(SimpleName node)
	{
		xml = xml + node.toString();
		return true;
	}
	@Override
	public boolean visit(Modifier node)
	{
		xml = xml + node.toString();
		return true;
	}
	@Override
	public boolean visit(PrimitiveType node)
	{
		xml = xml + node.toString();
		return true;
	}
	@Override
	public boolean visit(NumberLiteral node)
	{
		xml = xml + node.toString();
		return true;
	}	
	@Override
	public boolean visit(StringLiteral node)
	{
		xml = xml + node.toString();
		return true;
	}		
	@Override
	public boolean visit(InfixExpression node)
	{
		xml = xml + node.getOperator().toString();
		return true;
	}
	@Override
	public boolean visit(PrefixExpression node)
	{
		xml = xml + node.getOperator().toString();
		return true;
	}
	@Override
	public boolean visit(PostfixExpression node)
	{
		xml = xml + node.getOperator().toString();
		return true;
	}
	public TreeMakerVisitor()
	{
		xml = "";
	}
	@Override
	public void preVisit(ASTNode node)
	{
		xml = xml + "<" + getSimpleClassName(node.getClass().toString()) + ">";

	}
	
	@Override
	public void postVisit(ASTNode node)
	{
		xml = xml + "</" + getSimpleClassName(node.getClass().toString()) + ">";
	}
	
	private String getSimpleClassName(String classname)
	{
		String[] s = classname.split("\\.");
		return (s[s.length -1]);
	}
}
