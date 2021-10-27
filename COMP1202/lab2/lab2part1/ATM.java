public class ATM {

	Toolbox myToolbox = new Toolbox();
	int atmBalance = 0;

	/**
	 * Main function containing the go call.
	 *
	 * @param args Standard input arguments.
	 */
	public static void main(String[] args) {
		ATM myATM = new ATM();
		myATM.go();
	}

	/**
	 * Start sunction that gets input and outputs it.
	 */
	public void go() {
		System.out.println("Welcome to online ATM banking\n"
				+ "How much do you want in your account?");
		atmBalance = myToolbox.readIntegerFromCmd();
		System.out.println(atmBalance);
		return;
	}
}