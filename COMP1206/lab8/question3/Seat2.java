import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;
import java.util.concurrent.locks.ReentrantLock;

public class Seat2 implements Seat {
	private ReentrantLock leftFork;
	private ReentrantLock rightFork;

	@Override
	public synchronized void askFork1() {
		leftFork.lock();
	}

	@Override
	public synchronized void askFork2() {
		rightFork.lock();
	}

	@Override
	public synchronized void assignForks(ReentrantLock reentrantLockLeft, ReentrantLock reentrantLockRight) {
		leftFork = reentrantLockLeft;
		rightFork = reentrantLockRight;
	}
}