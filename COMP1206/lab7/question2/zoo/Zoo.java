package zoo;

import zoo.Counter;
import zoo.Gate;

public class Zoo {
  public static void main(String[] args) {
    Counter counter = new Counter();
    Runnable gate1 = new Gate(counter, 30000000);
    Runnable gate2 = new Gate(counter, 30000000);
    Runnable gate3 = new Gate(counter, 30000000);
    Runnable gate4 = new Gate(counter, 30000000);

    Thread gate1thread = new Thread(gate1);
    Thread gate2thread = new Thread(gate2);
    Thread gate3thread = new Thread(gate3);
    Thread gate4thread = new Thread(gate4);

    gate1thread.start();
    try {
      gate1thread.join(10);
    } catch (Exception e) {
      System.out.println(e);
    }

    gate2thread.start();
    try {
      gate2thread.join(1000);
    } catch (Exception e) {
      System.out.println(e);
    }

    gate3thread.start();
    try {
      gate3thread.join(10);
    } catch (Exception e) {
      System.out.println(e);
    }

    gate4thread.start();
    try {
      gate4thread.join(10);
    } catch (Exception e) {
      System.out.println(e);
    }

    System.out.println("" + counter.getCounter());
  }
}
