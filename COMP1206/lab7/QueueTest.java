import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.NumberQueue;

/**
 * Tests for the Lab on threading, exceptions and synchronisation. Question 2:
 * Cyclic queue and belt.
 *
 * @author jan
 *
 */
class QueueTest {

	private static int QUEUE_SIZE = 5;

	/**
	 * Test that an empty queue returns true on empty method and throws an exception
	 * on dequeue.
	 */
	@Test
	void queueEmptyExceptionTest() {
		NumberQueue queue = new CyclicQueue(QUEUE_SIZE);
		assertEquals(true, queue.isEmpty(), "Queue shaould be empty.");
		assertThrows(IndexOutOfBoundsException.class, () -> {
			queue.dequeue();
		});
	}

	/**
	 * Test whether a full queue throws an exception on enqueue.
	 */
	@Test
	void queueFullExceptionTest() {
		NumberQueue queue = fullQueue();
		assertThrows(IndexOutOfBoundsException.class, () -> {
			queue.enqueue(0);
		});
	}

	/**
	 * Tests that the right elements are returned from the queue and that the empty
	 * conditions are satisfied as well.
	 */
	@Test
	void queueRightValueTest() {
		NumberQueue queue = fullQueue();
		queue.dequeue();
		assertEquals(1, queue.dequeue(), "Wrong element from queue.");
		queue.dequeue();
		assertEquals(3, queue.dequeue(), "Wrong element from queue.");
		queue.enqueue(5);
		queue.enqueue(6);
		queue.enqueue(7);
		queue.dequeue();
		queue.dequeue();
		queue.dequeue();
		assertEquals(7, queue.dequeue(), "Wrong element from queue.");
		assertEquals(true, queue.isEmpty(), "Queue shaould be empty.");
		assertThrows(IndexOutOfBoundsException.class, () -> {
			queue.dequeue();
		});
	}

	/**
	 * Test that the queue works when enough elements are added and removes to reach
	 * the boundary of the array. Inserts and removes a number of
	 */
	@Test
	void queueRunTest() {
		NumberQueue queue = emptyQueue();
		double elemetsFactor = 5.5;
		int loopBoundary = (int) Math.floor(elemetsFactor * QUEUE_SIZE);
		queue.enqueue(-1);
		for (int i = 0; i < loopBoundary; i++) {
			queue.enqueue(i);
			queue.dequeue();
		}
		assertEquals(false, queue.isEmpty(), "Queue should contain one element.");
		int lastNumber = loopBoundary - 1;
		assertEquals(lastNumber, queue.dequeue(), "Wrong element in queue.");
		assertEquals(true, queue.isEmpty(), "Queue should be empty.");
	}

	/**
	 * Fills a queue with enough consecutive numbers to be full
	 * @return The queue with consecutive numbers.
	 */
	private NumberQueue fullQueue() {
		CyclicQueue queue = new CyclicQueue(QUEUE_SIZE);
		for (int i = 0; i < QUEUE_SIZE; i++) {
			queue.enqueue(i);
		}
		return queue;
	}

	/**
	 * Creates an empty queue.
	 * @return An empty queue.
	 */
	private NumberQueue emptyQueue() {
		CyclicQueue queue = new CyclicQueue(QUEUE_SIZE);
		return queue;
	}
}