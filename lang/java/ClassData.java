import java.util.HashSet;
import java.util.List;

import org.eclipse.jdt.core.dom.*;

public class ClassData {
	
	private HashSet<String> typesUsed;
	private String superClass;
	private HashSet<String> class_implements;
	private boolean isClass;
	private String name;
	
	public ClassData(TypeDeclaration node)
	{
		name = node.getName().toString();//get class name
		isClass = !node.isInterface();
		Type tmpSuper = node.getSuperclassType();//get the name of superclass
		if (tmpSuper == null) superClass = "Object" ;//if null is Object
		else superClass = tmpSuper.toString();
		
		List tmp = node.superInterfaceTypes();//get and convert interfaces
		class_implements = new HashSet<String>();
		for (Object o: tmp)
		{
			class_implements.add(((Type)o).toString());
		};
		typesUsed = new HashSet<String>();//prepare types used in string.
	}
	public boolean getIsClass() {
		return isClass;
	}
	public String getName() {
		return name;
	}
	public HashSet<String> getClass_implements() {
		return class_implements;
	}
	public String getClass_extends() {
		return superClass;
	}
	public HashSet<String> getTypesUsed() {
		return typesUsed;
	}
	public void addUseType(Type type) {
		if (type != null)
			typesUsed.add(type.toString());
		return;
	}
	/**

	 * @effects removes instances of the primitive types and wrapper classes, 
	 * String, and Void
	 */
	static String[] cleaned = 
			new String[]{"byte","short","int","long","float", "double","char","boolean",
				"void","String","Byte","Short","Integer","Long","Float","Double","Character","Boolean"};
	public void clean()
	{
		for (String s:cleaned)
		{
			typesUsed.remove(s);
		}
		typesUsed.remove(name);
	}
	
	public String toString()
	{	
		String ret = name + " isa " + superClass + "\n";
		
		for (String t: class_implements)
		{
			 ret = ret + name + " isa "  + t + "\n";
		}
		ret = ret + "\n";
		for(String t:typesUsed)
		{
			ret = ret + name + " hasa " + t + "\n";
		}
		return ret;
	}
}
