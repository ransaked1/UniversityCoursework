package bugs;

/** Concurent Modification Bug constructor with 20 base HP and 4 base Steps. */
public class ConcurrentModificationBug extends Bug {
  public ConcurrentModificationBug(String name, int level, int initialSteps) {
    super(name, 20, 4, level, initialSteps);
  }
}
