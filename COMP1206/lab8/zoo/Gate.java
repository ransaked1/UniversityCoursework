package zoo;

import zoo.Counter;

public class Gate implements Runnable {

  private Counter counter;
  private int guests;

  public Gate(Counter counter, int guests) {
    this.counter = counter;
    this.guests = guests;
  }

  public void run() {
    while (guests > 0) {
      counter.addOne();
      guests--;
    }
  }
}
