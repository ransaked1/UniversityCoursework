Task to develop UML for:

The NHS needs to build a COVID vaccination management system. Citizens need to book
appointments for vaccination at vaccination centres. Vaccine centre administrators need to
manage the number of available appointments and update the vaccine stock.

• A vaccine has a brand (e.g., “Pfizer”, “AstraZeneca”, “Novavax”, “Moderna”) and a batch
number. A batch number identifies vaccines created in the same factory batch. Each
batch number is unique.
• A citizen has a name, an NHS number, a date of birth, and a list of received shots, each
shot including vaccine information and date.
• A vaccination centre has a vaccine stock, that is, the number of available vaccines; a range
of working days, each day with the number of working hours; the number of available
appointments per working hour; and a list of booked appointments.

A citizen can book an appointment if they have received less than 3 shots and if their last
shot was more than 8 calendar weeks from the date of login. Citizens under 40 years old
can only book an appointment to get a vaccine of the brand “Pfizer”. A citizen is offered
the earliest available appointment in a centre that has in stock vaccines of the same brand as
those previously received. If the citizen rejects the offer, the system offers the following earliest
appointment, that might be on a different centre. This flow continues until either the citizen
confirms a booking, abandons the system or there are no more available appointments to offer.

A citizen can re-book an appointment up to 1 day before the date of the original one. Other
than that, re-booking follows the same rules as booking. After re-booking is confirmed, the
previous appointment becomes available.

A vaccination centre administrator can create and update vaccination centres. Updates
include vaccine stock, working days and hours and number of available appointments per hour.

A vaccination staff member, get the consent from the citizen, update the list of received shots
of a citizen after administering the vaccine. Vaccine stock needs to be automatically updated
after a staff member updates the list of received shots of a citizen.