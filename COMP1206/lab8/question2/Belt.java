import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

public class Belt extends CyclicQueue {

  public Belt(int size) {
    super(size);
  }

  @Override
  public synchronized void enqueue(int number) throws IndexOutOfBoundsException {
    if (tail - head + 1 == capacity)
      try {
        this.wait();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
    int next = tail + 1;
    array[next % capacity] = number;
    tail += 1;
    this.notify();
  }

  @Override
  public synchronized int dequeue() throws IndexOutOfBoundsException {
    if (isEmpty())
      try {
        this.wait();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
    int rez = array[head % capacity];
    head += 1;
    this.notify();
    return rez;
  }
}
