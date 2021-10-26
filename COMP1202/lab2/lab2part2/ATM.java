

public class ATM {

    Toolbox myToolbox = new Toolbox();
    int atmBalance = 0;
    int actionSelector = 0;

    public static void main(String[] args) {
        ATM myATM = new ATM();
        myATM.go();
    }

    public void go() {
        System.out.println("Welcome to online ATM banking\n" +
                "How much do you want in your account?");
        atmBalance = myToolbox.readIntegerFromCmd();
        System.out.println(atmBalance);

        selectOption();
        executeOption();
        return;
    }

    private void selectOption() {
        System.out.println("What do you want to do?\n" +
                "1 : Withdraw\n" +
                "2 : Deposit\n" +
                "3 : Inquire\n" +
                "4 : Quit");
        actionSelector = myToolbox.readIntegerFromCmd();
        return;
    }

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
                System.out.println("******************************************\n" +
                        "         GoodBye!\n" +
                        "******************************************");
                //System.exit(0);
                break;
            default:
                break;
        }
        return;
    }

    private void withdrawMoney() {
        System.out.println("*****************************************\n" +
                "              Withdrawal                 \n" +
                "*****************************************\n" +
                "How much would you like to withdraw?");
        int amount = myToolbox.readIntegerFromCmd();
        atmBalance -= amount;
        System.out.println("*****************************************\n" +
                "         Your new balance is " + atmBalance + "       \n" +
                "***************************************** ");
    }

    private void depositMoney() {
        System.out.println("*****************************************\n" +
                "              Deposit                 \n" +
                "*****************************************\n" +
                "How much would you like to deposit?");
        int amount = myToolbox.readIntegerFromCmd();
        atmBalance += amount;
        System.out.println("*****************************************\n" +
                "         Your new balance is " + atmBalance + "       \n" +
                "***************************************** ");
    }

    private void inquireAmount() {
        System.out.println("*****************************************\n" +
                "         Your balance is " + atmBalance + "       \n" +
                "***************************************** ");
    }
}