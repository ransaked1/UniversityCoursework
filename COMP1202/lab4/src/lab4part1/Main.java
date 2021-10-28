

class Main {

	Toolbox myToolbox = new Toolbox();
	int nrMultiply = 0;

	/**
	 * Multiplication with for loop.
	 *
	 * @param args Standard terminal input.
	 */
	public static void main(String[] args) {
		nrMultiply = myToolbox.readIntegerFromCmd();
		for(int i = 1; i <= 20; i++) {
			System.out.println("" + nrMultiply * i);
		}
	}
}