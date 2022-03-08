import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.*;

public class KPalindrome implements PalindromeChecker {
  public boolean isKPalindrome(String word, int k) {
    if (word.length() <= 1 && k >= 0) return true;
    if (word.charAt(0) == word.charAt(word.length() - 1))
      return isKPalindrome(word.substring(1, word.length() - 1), k);
    else {
      if (k == 0) return false;
      boolean rez = false;
      if (isKPalindrome(word.substring(0, word.length() - 1), k - 1)) rez = true;
      if (isKPalindrome(word.substring(1, word.length()), k - 1)) rez = true;
      return rez;
    }
  }
}
