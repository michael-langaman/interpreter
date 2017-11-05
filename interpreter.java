import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Stack;
import java.util.HashMap; 
import java.util.LinkedList;
import java.util.Queue;
import java.util.Arrays;
import java.util.Collections;

public class interpreter {
    
    public static void error(Stack<Element> stack, Element a, Element b) {
        stack.push(b);
        stack.push(a);
        stack.push(new Element(":error:"));
    }

    public static void push(Stack<Element> stack, Element e, HashMap<String, Element> binds, boolean funCall) {
        if(e.equals("-0")) stack.push(new Element(0));
        else if(e.isStr() && isDub(e.getStrVal())) {
            stack.push(new Element(":error:"));
        } else {
            stack.push(e);
        }
    }
    
    public static void pushFun(Stack<Element> stack, HashMap<String, Element> binds, Element function) {
        // System.out.println(function.getArgument().printVal());
        if(!function.isInOut() && function.getArgument().isName()) {
            if(binds.containsKey(function.getArgument().getName())) {
                stack.push(new Element(function.getArgument().getName()));
            } else {
                stack.push(function.getArgument());
            }
        } else stack.push(function.getArgument());
    }

    public static void pop(Stack<Element> stack) {
        if(stack.empty()) {
            stack.push(new Element(":error:"));
        } else {
            stack.pop();
        }
    }

    public static void neg(Stack<Element> stack, HashMap<String, Element> binds) {
        if(stack.empty()) {
            stack.push(new Element(":error:"));
        } else {
            Element a = stack.pop();
            if(a.isInt()) {
                int num = a.getIntVal();
                num *= -1;
                Element result = new Element(num);
                stack.push(result);
            } else if(a.isName()) {
                if(binds.containsKey(a.getName())) {
                    Element e = binds.get(a.getName());
                    int val = e.getIntVal();
                    val *= -1;
                    Element result = new Element(val);
                    stack.push(result);
                }
            }
        }
    }
    
    public static void swap(Stack<Element> stack) {
        if(stack.size() < 2) {
            stack.push(new Element(":error:"));
            return;
        } else {
            Element a = stack.pop();
            Element b = stack.pop();
            stack.push(a);
            stack.push(b);
        }
    }

    public static void arith(Stack<Element> stack, String oper, Element a, Element b) {
        boolean dne = false;
        int ans = -1024;
        int x = a.getIntVal();
        int y = b.getIntVal();
        if(a.isUnit()) a = new Element(a.getName());
        if(b.isUnit()) b = new Element(b.getName());
        if(oper.equals("add")) ans = x + y;
        else if(oper.equals("sub")) ans = y - x;
        else if(oper.equals("mul")) ans = x * y;
        else if(oper.equals("div")) { 
            if(x == 0) {
                stack.push(b);
                stack.push(a);
                stack.push(new Element(":error:"));
                dne = true;
            } else { ans = y / x; }
        } else if(oper.equals("rem")) {
            if(x == 0) {
                stack.push(b);
                stack.push(a);
                stack.push(new Element(":error:"));
                dne = true;
            } else { ans = y % x; }
        } else if(oper.equals("equal")) {
            boolean result =  x == y;
            stack.push(new Element(result));
            return;
        } else if(oper.equals("lessThan")) {
            boolean result = y < x;
            stack.push(new Element(result));
            return;
        }
        Element result = new Element(ans);
        // System.out.println(result.printVal());
        if(!dne) stack.push(result);
    }

    public static void math(Stack<Element> stack, String oper, HashMap<String, Element> binds) {
        if(stack.size() < 2) stack.push(new Element(":error:"));
        else {
            Element a = stack.pop();
            Element b = stack.pop();
            System.out.println("A: " + a.printVal() + " " + oper + " B: " + b.printVal());
            if(a.isUnit() || b.isUnit()) {
                // System.out.println("FUCK");
                error(stack, a, b);
            }
            else if(a.isInt() && b.isInt()) {
                arith(stack, oper, a, b);
            } else if(a.isName() && b.isName()) {
                Element c = search(a, binds);
                Element d = search(b, binds);
                if(binds.containsKey(a.getName()) && binds.containsKey(b.getName()) && c.isInt() && d.isInt()) {
                    System.out.println(c.getIntVal() + " - " + d.getIntVal());
                    arith(stack, oper, c, d);
                } else {
                    error(stack, a, b);
                }
            } else if(a.isName() && b.isInt()) {
                if(binds.containsKey(a.getName())) {
                    Element e = search(a, binds);
                    if(e.isInt()) arith(stack, oper, e, b);
                    else error(stack, a, b);
                } else {
                    error(stack, a, b);
                }
            } else if(a.isInt() && b.isName()) {
                if(binds.containsKey(b.getName())) {
                    Element e = search(b, binds);
                    if(e.isInt()) arith(stack, oper, a, e);
                    else {
                        error(stack, a, b);
                    }
                } else {
                    error(stack, a, b);
                }
            } else {
                error(stack, a, b);
            }
        }
    }

    public static void boolHelper(Stack<Element> stack, String command, Element a, Element b) {
        boolean x = a.getBoolVal();
        boolean y = b.getBoolVal();
        boolean result = true;
        if(command.equals("and")) result = x && y;
        else result = x || y;
        stack.push(new Element(result));
    }   

    public static void bool(Stack<Element> stack, String command, HashMap<String, Element> binds) {
        if(stack.size() < 2) stack.push(new Element(":error:"));
        else {
            Element a = stack.pop();
            Element b = stack.pop();
            if(a.isUnit() || b.isUnit()) {
                error(stack, a, b);
                return;
            }
            if(a.isBool() && b.isBool()) boolHelper(stack, command, a, b);
            else if(a.isName() && b.isName()) {
                if(binds.containsKey(a.getName()) && binds.containsKey(b.getName())) {
                    Element c = binds.get(a.getName());
                    Element d = binds.get(b.getName());
                    if(c.isBool() && d.isBool()) boolHelper(stack,command, c, d);
                    else error(stack, a, b);
                } else {
                    error(stack, a, b);
                }
            } else if(a.isBool() && b.isName()) {
                if(binds.containsKey(b.getName())) {
                    Element e = binds.get(b.getName());
                    if(e.isBool()) boolHelper(stack, command, a, e);
                    else error(stack, a, b);
                } else error(stack, a, b);
            } else if(a.isName() && b.isBool()) {
                if(binds.containsKey(a.getName())) {
                    Element e = binds.get(a.getName());
                    if(e.isBool()) boolHelper(stack, command, b, e);
                    else error(stack, a, b);
                } else error(stack, a, b);
            } else {
                error(stack, a, b);
            }
        }
    }

    public static void not(Stack<Element> stack, HashMap<String, Element> binds) {
        if(stack.empty()) stack.push(new Element(":error:"));
        else {
            Element a = stack.pop();
            if(a.isBool()) {
                boolean x = a.getBoolVal();
                boolean result = !x;
                stack.push(new Element(result));    
            } else if(a.isName()) {
                if(binds.containsKey(a.getName())) {
                    Element e = binds.get(a.getName());
                    if(e.isBool()) {
                        boolean x = e.getBoolVal();
                        boolean result = !x;
                        stack.push(new Element(result));
                    } else {
                        stack.push(a);
                        stack.push(new Element(":error:"));
                    }
                } else {
                    stack.push(a);
                    stack.push(new Element(":error:"));
                }
            } else {
                stack.push(a);
                stack.push(new Element(":error:"));
            }
        }
    }

    public static Element search(Element a, HashMap<String, Element> binds) {
        return binds.get(a.getName());
    }   

    public static void bind(Stack<Element> stack, HashMap<String, Element> binds) {
        if(stack.size() < 2) stack.push(new Element(":error:"));
        else {
            Element a = stack.pop();
            Element b = stack.pop();
            if(b.isName()) {
                String name = b.getName();
                String type = a.getType();
                Element result = null;
                switch(type) {
                case "string":
                    result = new Element(name, a.getStrVal());
                    break;
                case "int":
                    result = new Element(name, a.getIntVal());
                    break;
                case "bool":
                    result = new Element(name, a.getBoolVal());
                    break;
                case "name":
                    if(!binds.containsKey(a.getName())) error(stack, a, b);
                    else {
                        result = new Element(name, binds.get(a.getName()));
                    }
                    break;
                default: 
                    error(stack, a, b);
                    break;
                }
                if(result != null) {
                    binds.put(name, result);
                    stack.push(result);
                }
            } else {
                System.out.println(a.printVal() + " - " + b.printVal());
                error(stack, a, b);
            }
        }
    }

    public static void if_(Stack<Element> stack, HashMap<String, Element> binds) {
        if(stack.size() < 3) stack.push(new Element(":error:"));
        else {
            Element a = stack.pop();
            Element b = stack.pop();
            Element c = stack.pop();
            // System.out.println("IF -> A: " + a.printVal() + " B : " + b.printVal() + " C: " + c.printVal());
            if(c.isBool()) {
                if(c.getBoolVal()) stack.push(a);
                else stack.push(b);
            } else if(c.isName()) {
                if(binds.containsKey(c.getName())) {
                    Element e = binds.get(c.getName());
                    if(e.isBool()) {
                        if(e.getBoolVal()) stack.push(a);
                        else stack.push(b);
                    } else error(stack, a, b);
                } else error(stack, a, b);
            } else {
                stack.push(c);
                stack.push(b);
                stack.push(a);
                stack.push(new Element(":error:"));
            }
        }
    }

    public static Stack<Element> let(Stack<Element> stack, HashMap<String, Element> binds, Queue<String> lines, String outFile, boolean funCall, Element function) {
        while(!lines.isEmpty()) {
            String line = lines.remove();
            String command = command(line);
            String value = value(line);
            Element e = element(value);
            if(command.equals("end")) {
                // printStack(stack);
                return stack;
            }
            else interpret(stack, binds, lines, command, e, outFile, true, funCall, function);
        }
        return stack;
    }

    public static Element fun(String functionName, String argName, HashMap<String, Element> binds, Queue<String> lines, boolean inOut, boolean nested) {
        Queue<String> code = new LinkedList<String>();
        while(!lines.isEmpty()) {
            String line = lines.remove();
            // System.out.println(line);
            String[] values = line.split(" ");
            if(values.length > 0 && values[0].equals("fun")) {
                String funName = values[1];
                String param = values[2];
                Element nest = fun(funName, param, binds, lines, inOut, true);
            } else if(line.equals("funEnd")) {
                code.add(line);
                HashMap<String, Element> temp = new HashMap<String, Element>(binds);
                Element function = new Element(functionName, argName, code, temp, inOut);
                binds.put(functionName, function);
                return function;
            } else {
                code.add(line);
            }
        }
        return null;
    }

    public static void setElem(Element a, Element b) {
        switch(b.getType()) {
            case "string":
            a = new Element(b.getStrVal());
            break;
            case "int":
            a = new Element(b.getIntVal());
            break;
            case "bool":
            a = new Element(b.getBoolVal());
            break;
        }
    } 

    public static Stack<Element> call(Element function, Stack<Element> stack, HashMap<String, Element> binds, Queue<String> lines, String outFile, boolean local, boolean funCall) {
        Queue<String> code = new LinkedList<String>(function.getCode());
        Element temp = function;
        // System.out.println(function.getName());
        while(!code.isEmpty()) {
            String line = code.remove();
            // System.out.println(line);
            String command = command(line);
            String value = value(line);
            Element e = element(value);
            if(command.equals("return")) {
                return stack;
            } else if(command.equals("funEnd")) {
                return null;
            }
            else interpret(stack, binds, code, command, e, outFile, local, true, function);
        }
        return stack;
    }

    // public static Element inOutCall(Element function, Stack<Element> stack, HashMap<String, Element> binds, Queue<String> lines, String outFile, boolean local, boolean funCall) {
    //     Queue<String> code = new LinkedList<String>(function.getCode());
    //     Element argument = function.getArgument();
    //     while(!code.isEmpty()) {
    //         String line = code.remove();
    //         String command = command(line);
    //         String value = value(line);
    //         Element e = element(value);
    //         System.out.println(command);
    //         if(command.equals("return")) {
    //             Element top = stack.pop();
    //         }
    //         else interpret(stack, binds, code, command, e, outFile, true, true, function);
    //     }
    //     return stack.pop();
    // }

    public static LinkedList<String> getLines(String inFile) {
        LinkedList<String> lines = new LinkedList<String>();
        try {
            FileReader read = new FileReader(inFile);
            BufferedReader buffR = new BufferedReader(read);
            String line = null;
            while((line = buffR.readLine()) != null) {
                lines.add(line);
            }
            buffR.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return lines;
    }
    
    public static void writeOutput(Stack<Element> stack, String outFile) {
        BufferedWriter buffW = null;
        try {
            FileWriter write = new FileWriter(outFile);
            buffW = new BufferedWriter(write);
            int count = 0;
            while(!stack.empty()) {
                Element p = stack.pop();
                buffW.write(p.getOutput());
                buffW.newLine();
                ++count;
            }
            buffW.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
    
    public static boolean isNum(String a) {
        try {
            Integer.parseInt(a);
            return true;
        } catch(NumberFormatException ex) {
            return false;
        }
    }

    public static boolean isDub(String a) {
        try {
            Double.parseDouble(a);
            return true;
        } catch(NumberFormatException ex) {
            return false;
        }
    }

    public static Element element(String value) {
        Element e = null;
        if(isNum(value)) {
            e = new Element(Integer.parseInt(value));
        } else {
            e = new Element(value);
        }
        return e;
    }

    public static String command(String line) {
        String[] s = line.split(" ");
        return s[0];
    }

    public static String value(String line) {
        String[] s = line.split(" ");   
        String value = "";
        if(s.length == 2) {
            value = s[1];
        } else if(s.length > 2) {
            value += s[1];
            for(int j = 2; j < s.length; ++j) {
                value = value + " " + s[j];
            }
        } else {}
        return value;
    }

    public static void printStack(Stack<Element> s) {
        Stack<Element> stack = new Stack<Element>();
        stack.addAll(s);
        String st = "";
        while(!stack.empty()) {
            Element a = stack.pop();
            st += a.printVal();
        }
        System.out.println(st + " | ");
    }

    public static void interpret(Stack<Element> stack, HashMap<String, Element> binds, Queue<String> lines, String command, Element e, String outFile, boolean local, boolean funCall, Element funct) {
        // System.out.println(command + " - " + e.printVal());
        switch(command) {
            case "push":
                if(funCall && e.isName() && e.getName().equals(funct.getArgumentName())) {
                     pushFun(stack, binds, funct);
                } else {
                    push(stack, e, binds, funCall);
                }
                break;
            case "pop":
                pop(stack);
                break;
            case ":true:":
                stack.push(new Element(true));
                break;
            case ":false:":
                stack.push(new Element(false));
                break;
            case ":error:":
                stack.push(new Element(command));
                break;
            case "add":
                math(stack, command, binds);
                break;
            case "sub":
                math(stack, command, binds);
                break;
            case "mul":
                math(stack, command, binds);
                break;
            case "div":
                math(stack, command, binds);
                break;
            case "rem":
                math(stack, command, binds);
                break;
            case "neg":
                neg(stack, binds);
                break;
            case "swap":
                swap(stack);
                break;
            case "and":
                bool(stack, command, binds);
                break;
            case "or":
                bool(stack, command, binds);
                break;
            case "not":
                not(stack, binds);
                break;
            case "equal":
                math(stack, command, binds);
                break;
            case "lessThan":
                math(stack, command, binds);
                break;
            case "bind":
                bind(stack, binds);
                break;
            case "if":
                if_(stack, binds);
                break;
            case "let":
                Stack<Element> temp = new Stack<Element>();
                if(!local) {
                    HashMap<String, Element> tempBind = new HashMap<String, Element>();
                    Stack<Element> s = let(temp, tempBind, lines, outFile, funCall, funct);
                    Element a = s.pop();
                    stack.push(a);
                } else {
                    System.out.println("falco");
                    HashMap<String, Element> save = new HashMap<String, Element>(binds);
                    Stack<Element> s = let(temp, save, lines, outFile, funCall, funct);
                    Element a = s.pop();
                    // System.out.println(Arrays.asList(binds));
                    // binds = new HashMap<String, Element>(save);
                    // System.out.println(Arrays.asList(binds));
                    stack.push(a);
                }
                break;
            case "fun":
                String s = e.printVal();
                String[] values = s.split(" ");
                String functionName = values[0];
                String argName = values[1];
                Element f = fun(functionName, argName, binds, lines, false, false);
                stack.push(f);
                break;
            case "call":
                Element function = stack.pop();
                Element save = function;
                Element argument = stack.pop();
                String funName = function.getName();
                if(binds.containsKey(funName) && binds.get(funName).isFunction() && !argument.isError()) {
                    function = binds.get(funName);
                    if(!function.isInOut()) {
                        // function = binds.get(funName);
                        HashMap<String, Element> env = new HashMap<String, Element>(function.getEnv());
                        env.put(function.getName(), function);
                        if(!argument.isUnit() && argument.isName() && !binds.containsKey(argument.getName())) {
                            error(stack, save, argument);
                            return;
                        }
                        else if(argument.isName() && !argument.isUnit()) {
                            if(binds.containsKey(argument.getName())) {
                                Element ex = binds.get(argument.getName());
                                System.out.println(argument.getName() + " - " + ex.getType());
                                System.out.println(Arrays.asList(env));
                                if(!ex.isFunction()) {
                                    switch(ex.getType()) {
                                        case "int":
                                        env.put(argument.getName(), new Element(ex.getIntVal()));
                                        // argument = new Element(ex.getIntVal());
                                        break;
                                        case "string":
                                        env.put(argument.getName(), new Element(ex.getStrVal()));
                                        // argument = new Element(ex.getStrVal());
                                        // env.put(argument.getName(), new Element(ex.getStrVal()));
                                        break;
                                        case "bool":
                                        env.put(argument.getName(), new Element(ex.getBoolVal()));
                                        // argument = new Element(ex.getBoolVal());
                                        break;
                                    }
                                    // System.out.println(ex.printVal());
                                }
                            } else {
                                error(stack, function, argument);
                                return;
                            }
                        } else {}
                        function.setArgument(argument);
                        Stack<Element> funResult = null;
                        // System.out.println(function.isInOut());
                        if(funCall) {
                            // System.out.println(Arrays.asList(binds));
                            funResult = call(new Element(function), new Stack<Element>(), binds, lines, outFile, local, funCall);
                        } else {
                            // function.setArgument(argument);
                            funResult = call(new Element(function), new Stack<Element>(), env, lines, outFile, local, funCall);
                        }
                        if(funResult == null) return;
                        Element top = funResult.pop();
                        Element res = null;
                        System.out.println(function.getArgumentName() + " - " + top.getName());
                        if(top.isName() && !top.isUnit()) {
                            if(binds.containsKey(top.getName()) && !top.getName().equals(function.getArgumentName())) {
                                Element lookup = binds.get(top.getName());
                                if(lookup.isFunction()) {
                                    stack.push(top);
                                    return;
                                }
                                switch(lookup.getType()) {
                                    case "int":
                                    res = new Element(lookup.getIntVal());
                                    break;
                                    case "string":
                                    res = new Element(lookup.getStrVal());
                                    break;
                                    case "bool":
                                    res = new Element(lookup.getBoolVal());
                                    break;
                                }
                            } else if(top.getName().equals(function.getArgumentName())) res = function.getArgument();
                            else { res = top; }
                        } else { res = top; }
                        if(res != null) {
                            stack.push(res);
                        }
                    } else {
                        function.setArgument(argument);
                        Stack<Element> res = null;
                        res = call(function, new Stack<Element>(), binds, lines, outFile, local, funCall);
                        if(res == null) return;
                        Element head = res.pop();
                        Element ans = null;
                        if(head.isName() && !head.isUnit()) {
                            if(head.getName().equals(function.getArgumentName())) ans = function.getArgument();
                            else if(binds.containsKey(head.getName())) {
                                Element lookup = binds.get(head.getName());
                                switch(lookup.getType()) {
                                    case "int":
                                    ans = new Element(lookup.getIntVal());
                                    break;
                                    case "string":
                                    ans = new Element(lookup.getStrVal());
                                    break;
                                    case "bool":
                                    ans = new Element(lookup.getBoolVal());
                                    break;
                                }
                            } else { ans = head; }
                        } else { ans = head; }
                        if(ans != null) {
                            // System.out.println(ans.printVal());
                            stack.push(ans);
                        }
                    }
                } else {
                    error(stack, function, argument);
                }
                break;
            case "inOutFun":
                String str = e.printVal();
                String[] line = str.split(" ");
                String name = line[0];
                String parameter = line[1];
                Element ioFun = fun(name, parameter, binds, lines, true, false);
                stack.push(ioFun);
                break;
            case "quit":
                writeOutput(stack, outFile);
                break;
            }
    }

    public static void interpreter(String inFile, String outFile) {
        Stack<Element> stack = new Stack<Element>();
        HashMap<String, Element> binds = new HashMap<String, Element>();
        Queue<String> lines = getLines(inFile);
        while(!lines.isEmpty()) {
            String line = lines.remove();
            String command = command(line);
            String value = value(line);
            Element e = element(value);
            interpret(stack, binds, lines, command, e, outFile, false, false, null);
        }
    }
}