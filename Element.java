import java.util.LinkedList;
import java.util.Queue;
import java.util.HashMap; 

public class Element {

	private String _str;
	private int _num;
	private String _name;
	private String _bool;
	private boolean _error = false;
	private boolean _unit;
	private boolean _function;
	private Element _argument;
	private String _argName;
	private Queue<String> _code;
	private static int NULL = -1024;
	private HashMap<String, Element> _env;
	public boolean _inOut;

	public Element() {
		_str = null;
		_num = NULL;
		_name = null;
		_bool = null;
		_unit = false;
	}

	public Element(int num) {
		_str = null;
		_num = num;
		_name = null;
		_bool = null;
		_unit = false;
	}

	public Element(String str) {
		if(!str.equals("") && str.charAt(0) == '\"' && str.charAt(str.length() - 1) == '\"') {
			_str = str.replace("\"", "");
			_name = null;
		} else if(str.equals(":error:")) {
			_error = true;
		} else {
			if(str.length() > 0 && nameFlag(str)) {
				_name = str;
			} else {
				_error = true;
			}
			_str = null;
		}
		_num = NULL;
		_bool = null;
		_unit = false;
	}

	public Element(boolean b) {
		if(b) _bool = ":true:";
		else _bool = ":false:";
		_str = null;
		_num = NULL;
		_name = null;
		_unit = false;
	}

	public Element(String name, int num) {
		_name = name;
		_num = num;
		_bool = null;
		_str = null;
		_unit = true;
	}

	public Element(String name, String str) {
		_name = name;
		_str = str;
		_num = NULL;
		_bool = null;
		_unit = true;
	}

	public Element(String name, boolean b) {
		_name = name;
		_str = null;
		_num = NULL;
		if(b) _bool = ":true:";
		else _bool = ":false:";
		_unit = true;
	}

	public Element(String name, Element b) {
		_name = name;
		_unit = true;
		// System.out.println(b.getType());
		switch(b.getType()) {
			case "string":
				_str = b.getStrVal();
				_num = NULL;
				break;
			case "int":
				_num = b.getIntVal();
				break;
			case "bool":
				if(b.getBoolVal()) _bool = ":true:";
				else _bool = ":false:";
				_num = NULL;
				break;
			case "function":
				_function = true;
				_argName = b.getArgumentName();
				_code = b.getCode();
				_env = b.getEnv();
				_argument = b.getArgument();
				break;
		}
	}

	public Element(String name, String argName, Queue<String> code, HashMap<String, Element> env, boolean inOut) {
		_unit = true;
		_name = name;
		_num = NULL;
		_function = true;
		_argName = argName;
		_code = code;
		_env = new HashMap<String, Element>(env);
		_inOut = inOut;
	}

	public Element(Element e) {
		_unit = e.isUnit();
		_name = e.getName();
		_function = e.isFunction();
		_argName = e.getArgumentName();
		_code = e.getCode();
		_env = e.getEnv();
		_inOut = e.isInOut();
		_num = e.getIntVal();
		_str = e.getStrVal();
		_argument = e.getArgument();
	}

	public void setCode(Queue<String> code) {
		_code = code;
	}

	public Queue<String> getCode() {
		return _code;
	}

	public String getType() {
		if(isStr()) return "string";
		else if(isInt()) return "int";
		else if(isBool()) return "bool";
		else if(isFunction()) return "function";
		else if(isUnit()) return ":unit:";
		else if(isName()) return "name";
		return ":error:";
	}

	public String getOutput() {
		if(_unit) return ":unit:";
		else if(_function) return ":unit:";
		else if(_error) return ":error:";
		else if(isStr()) return _str;
		else if(isInt()) return Integer.toString(_num); 
		else if(isBool()) return _bool;
		else if(isName()) return _name;
		return "";
	}

	public String printVal() {
		String val = "";
		switch(getType()) {
			case "string":
			val = _str;
			break;
			case "int":
			val = Integer.toString(_num);
			break;
			case "bool":
			val = _bool;
			break;
			case "name":
			val = _name;
			break;
			case ":unit:":
			val = ":unit:";
			break;
			default:
			val = ":error:";
			break;
		}
		return val;
	}

	public void eval(Element a) {
		switch(a.getType()) {
		case "string":
			_str = a.getStrVal();
			break;
		case "int":
			_num = a.getIntVal();
			break;
		case "bool":
			if(a.getBoolVal()) _bool = ":true:";
			else _bool = ":false:";
			break;
		}
	}

	public String getUnitVal() {
		if(isStr()) return "STRING" + " - " +  _str + " - " + _name;
		else if(isBool()) return "BOOL" + " - " + _bool + " - " + _name;
		else return "NUM" + " - " + _num + " - " + _name;
	}

	public boolean isInt() {
		if(_num != NULL) {
			return _num == (int) _num;
		}
		return false;
	}

	public boolean isStr() {
		return _str != null;
	}

	public boolean isBool() {
		if(_bool != null) return _bool.equals(":true:") || _bool.equals(":false:");
		return false;
	}

	public boolean isFunction() {
		return _function;
	}

	public boolean hasArg() {
		return _argument != null;
	}


	public boolean nameFlag(String name) {
		if(name != null) {
			if(Character.isDigit(name.charAt(0))) return false;
			else {
				for(int i = 0; i < name.length(); ++i) {
					return Character.isDigit(name.charAt(i)) || Character.isLetter(name.charAt(i)) || Character.isWhitespace(name.charAt(i));
				}
			}
		} 
		return false;

	}

	public boolean isName() {
		return nameFlag(_name);
	}

	public boolean isUnit() {
		return _unit;
	}

	public boolean isError() {
		return _error;
	}

	public int getIntVal() {
		return _num;
	}

	public String getStrVal() {
		return _str;
	}

	public String getName() {
		return _name;
	}

	public Element getArgument() {
		return _argument;
	}

	public void setArgument(Element arg) {
		_argument = arg;
	}

	public int getArgIntVal() {
		return _num;
	}

	public void setArgumentValue(Element val) {
		switch(val.getType()) {
			case "int":
			_argument = new Element(_argument.getName(), val.getIntVal());
		}
	}

	public String getArgumentName() {
		return _argName;
	}

	public String getBool() {
		return _bool;
	}

	public boolean getBoolVal() {
		if(isBool()) {
			if(_bool.equals(":true:")) return true;
		}
		return false;
	}

	public HashMap<String, Element> getEnv() {
		return _env;
	}

	public void setEnv(HashMap<String, Element> env) {
		_env = new HashMap<String, Element>(env);
	}

	public void addToEnv(String s, Element e) {
		_env.put(s, e);
	}

	public boolean isInOut() {
		return _inOut;
	}

}