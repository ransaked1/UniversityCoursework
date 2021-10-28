package lab3part3;

public class SmartCard {

	String ownerName;
	Boolean isStaffMember;

	/**
	 * Constructor setting the owner of the smart card and defaulting to student card.
	 *
	 * @param name Name of the person owning the card.
	 */
	public SmartCard(String name) {
		ownerName = name;
		isStaffMember = false;
	}

	/**
	 * Return card owner.
	 *
	 * @return ownerName.
	 */
	public String getOwner() {
		return ownerName;
	}

	/**
	 * Check if owner is staff member.
	 *
	 * @return boolean isStaffMember.
	 */
	public Boolean isStaff() {
		return isStaffMember;
	}

	/**
	 * Set owner staff status.
	 *
	 * @param value boolean to set the status to.
	 */
	public void setStaff(Boolean value) {
		isStaffMember = value;
	}
}
