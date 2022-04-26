import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

public class Counter implements UnitCounter {
	private int counter = 0;

	public int getCounter() {
		return counter;
	}

	public void addOne() {
		counter++;
	}
}