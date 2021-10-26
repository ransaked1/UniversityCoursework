

public class ATM {

    Toolbox myToolbox = new Toolbox();
    int atmBalance = 0;

    public static void main(String[] args) {
        ATM myATM = new ATM();
        myATM.go();
    }

    public void go() {
        System.out.println("Welcome to online ATM banking\n" +
                "How much do you want in your account?");
        atmBalance = myToolbox.readIntegerFromCmd();
        System.out.println(atmBalance);
        return;
    }
}