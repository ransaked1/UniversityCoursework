package lab4part1;

public class Main {

  static Toolbox myToolbox = new Toolbox();
  private static Integer nrMultiply;

  /**
   * Multiplication with for loop and count iterations.
   *
   * @param args Standard terminal input.
   */
  public static void main(String[] args) {
    nrMultiply = myToolbox.readIntegerFromCmd();
    for (int i = 1; i <= 20; i++) {
      System.out.println("" + nrMultiply * i);
    }

    int sum = 0;
    int nr = 1;
    while (sum < 500) {
      sum += nr;
      nr += 1;
    }
    System.out.println(nr - 1);
  }
}
