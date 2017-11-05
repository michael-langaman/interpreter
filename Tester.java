public class Tester {
	public static void main(String[] args) {
		String input = "input.txt";
		String output = "output.txt";
		/* You can also read input and output as command line arguments*/
		if(args.length > 0) {
			input = "input_" + args[0] + ".txt";
		}
		interpreter.interpreter(input, output);
	}
}