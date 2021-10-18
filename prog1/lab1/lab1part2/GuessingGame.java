public class GuessingGame{

	public static void main(String[] args){
		//Declaring variables and initailizing Toolbox object
		Integer numberToGuess;
		Integer guessedNumber;

		Toolbox myToolbox = new Toolbox();

		//Print welcome message at start
		System.out.println("Welcome to Guessing Game");

		//Generating a number to guess
		numberToGuess = myToolbox.getRandomInteger(10);

		//Making a guess
		guessedNumber = myToolbox.readIntegerFromCmd();

		//If guess is right print right
		if (numberToGuess.equals(guessedNumber)){
			System.out.println("right");
		} 
		else if (numberToGuess > guessedNumber){
			System.out.println("too low");			//If guess is lower
		}
		else {
			System.out.println("too high");			//If guess is higher
		}

		return ;
	}
}
