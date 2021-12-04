package bugs;

/**
 * Null Pointer Bug constructor with 200 base HP and 6 base Steps.
 */
public class NullPointerBug extends Bug {
	public NullPointerBug(String name, int level, int initialSteps) {
		super(name, 10, 2, level, initialSteps);
	}
}