import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;
import uk.ac.soton.ecs.comp1206.labtestlibrary.recursion.*;

public class Factory implements SeatFactory {
	@Override
	public Tuple<Class<? extends Seat>, Class<? extends Seat>> getSeats() {
		Tuple tuple = new Tuple(Seat1.class, Seat2.class);
		return tuple;
	}
}