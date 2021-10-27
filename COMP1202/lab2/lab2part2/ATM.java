public class ATM {

	Toolbox myToolbox = new Toolbox();
	int atmBalance = 0;
	int actionSelector = 0;

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
	 * Controller function.
	 */
	public void go() {
		System.out.println("Welcome to online ATM banking\n"
				+ "How much do you want in your account?");
		atmBalance = myToolbox.readIntegerFromCmd();
		System.out.println(atmBalance);

		selectOption();
		executeOption();
		return;
	}

	/**
	 * Option selector function.
	 */
	private void selectOption() {
		System.out.println("What do you want to do?\n"
				+ "1 : Withdraw\n"
				+ "2 : Deposit\n"
				+ "3 : Inquire\n"
				+ "4 : Quit");
		actionSelector = myToolbox.readIntegerFromCmd();
		return;
	}

	/**
	 * Execute the option selected previously.
	 */
	private void executeOption() {
		switch (actionSelector) {
			case 1:
				withdrawMoney();
				break;
			case 2:
				depositMoney();
				break;
			case 3:
				inquireAmount();
				break;
			case 4:
				System.out.println("******************************************\n"
						+ "         GoodBye!\n"
						+ "******************************************");
				//System.exit(0);
				break;
			default:
				break;
		}
		return;
	}

	/**
	 * Withdraw money option.
	 */
	private void withdrawMoney() {
		System.out.println("*****************************************\n"
				+ "              Withdrawal                 \n"
				+ "*****************************************\n"
				+ "How much would you like to withdraw?");
		int amount = myToolbox.readIntegerFromCmd();
		atmBalance -= amount;
		System.out.println("*****************************************\n"
				+ "         Your new balance is " + atmBalance + "       \n"
				+ "***************************************** ");
	}

	/**
	 * Deposit money option.
	 */
	private void depositMoney() {
		System.out.println("*****************************************\n"
				+ "              Deposit                 \n"
				+ "*****************************************\n"
				+ "How much would you like to deposit?");
		int amount = myToolbox.readIntegerFromCmd();
		atmBalance += amount;
		System.out.println("*****************************************\n"
				+ "         Your new balance is " + atmBalance + "       \n"
				+ "***************************************** ");
	}

	/**
	 * Get the balance printed out.
	 */
	private void inquireAmount() {
		System.out.println("*****************************************\n"
				+ "         Your balance is " + atmBalance + "       \n"
				+ "***************************************** ");
	}
}