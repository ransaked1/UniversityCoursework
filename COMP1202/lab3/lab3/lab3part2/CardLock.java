package lab3part2;

public class CardLock {

	SmartCard lastCard;
	Boolean isUnlockedStatus = false;
	Boolean allowStudentAccess = false;

	/**
	 * Swipe card and change lock status.
	 *
	 * @param card the smart card to swipe.
	 */
	public void swipeCard(SmartCard card) {
		lastCard = card;
		if (lastCard.isStaff() || allowStudentAccess == true) {
			isUnlockedStatus = true;
		} else {
			isUnlockedStatus = false;
		}
		return;
	}

	/**
	 * Get the last card swiped.
	 *
	 * @return SmartCard object;
	 */
	public SmartCard getLastCardSeen() {
		return lastCard;
	}

	/**
	 * Check lock state.
	 *
	 * @return boolean isUnlockedStatus.
	 */
	public Boolean isUnlocked() {
		return isUnlockedStatus;
	}

	/**
	 * Toggle method that changes the allowStudentAccess variable.
	 */
	public void toggleStudentAccess() {
		allowStudentAccess = !allowStudentAccess;
	}

}
