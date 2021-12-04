package bugs;

/**
 * None Termination Bug constructor with 200 base HP and 6 base Steps.
 */
public class NoneTerminationBug extends Bug {
	public NoneTerminationBug(String name, int level, int initialSteps) {
		super(name, 200, 6, level, initialSteps);
	}
}