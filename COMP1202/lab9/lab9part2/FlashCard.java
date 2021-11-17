public class FlashCard {

  String question;
  String answer;

  public FlashCard(String question, String answer) {
    this.question = question;
    this.answer = answer;
  }

  /**
   * Return the questions.
   *
   * @return The questions in the form of a string.
   */
  public String getQuestion() {
    return question;
  }

  /**
   * Return the answer.
   *
   * @return The answer in the form of a string.
   */
  public String getAnswer() {
    return answer;
  }
}
