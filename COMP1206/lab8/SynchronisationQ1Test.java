/** */
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;
import zoo.*;

import org.junit.jupiter.api.Test;

/**
 * Tests for the Lab on threading, exceptions and synchronisation. Question 1: Zoo
 *
 * @author jan
 */
class SynchronisationQ1Test {
  /**
   * Tests whether the counter has counted every guest.
   *
   * @throws InterruptedException
   */
  @Test
  void zooTest() throws InterruptedException {
    Counter counter = new Counter();
    int numberGates = 20;
    int numberGuestsPerGate = 5000;
    List<Gate> allGates = new ArrayList<>();
    List<Thread> allGateThreads = new ArrayList<>();
    for (int i = 0; i < numberGates; i++) {
      allGates.add(new Gate(counter, numberGuestsPerGate));
      Thread gateThread = new Thread(allGates.get(i));
      allGateThreads.add(gateThread);
      gateThread.start();
    }
    for (int i = 0; i < numberGates; i++) {
      allGateThreads.get(i).join();
    }
    assertEquals(
        numberGates * numberGuestsPerGate, counter.getCounter(), "Wrong number of guests counted.");
  }
}
