import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

public class CyclicQueue implements NumberQueue {

	int capacity;
	int[] array;
	int head = 0;
	int tail = -1;

	public CyclicQueue(int capacity) {
		this.capacity = capacity;
		array = new int[capacity];
	}

	public void enqueue(int number) throws IndexOutOfBoundsException {
		if (tail - head + 1 == capacity)
			throw new IndexOutOfBoundsException("Index out of bounds. Queue is full.");
		int next = tail + 1;
		array[next % capacity] = number;
		tail += 1;
	}

	public int dequeue() throws IndexOutOfBoundsException {
		if (isEmpty())
			throw new IndexOutOfBoundsException("Index out of bounds. Queue is empty.");
		int rez = array[head % capacity];
		head += 1;
		return rez;
	}

	public boolean isEmpty() {
		if (head > tail)
			return true;
		return false;
	}

}