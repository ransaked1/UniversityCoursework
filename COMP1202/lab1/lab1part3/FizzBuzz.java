//Declaring the object FizzBuzz and the method main
public class FizzBuzz {
	/**
	 * Program that prints a number/Fizz/Buzz depending on the number.
	 *
	 * @param args Standard terminal input.
	 */
	public static void main(String[] args) {
		//For loop that iterates from 1 to 61
		for(Integer i = new Integer(1); i < 61; i++) {
			//If the number is not divisible by 3 and 5 print it
			if(i % 3 != 0 && i % 5 != 0) {
				System.out.print(i);
			}

			//If the number is divisible by 3 print "Fizz"
			if(i % 3 == 0) {
				System.out.print("Fizz");
			}
			
			//If the number is divisible by 5 print "Buzz"
			if(i % 5 == 0) {
				System.out.print("Buzz");
			}

			//Put a new line after every output from above
			System.out.println();
		}
	}
}
