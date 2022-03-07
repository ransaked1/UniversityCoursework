import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.*;

public class MinInt implements MinimumInArray {
  public int findMin(int[] array) {
		if (array.length == 0) return 0;

    if (array.length == 1) return array[0];
    else {
      int[] newarray = new int[array.length - 1];

			int compare = array[0];
      System.arraycopy(array, 1, newarray, 0, newarray.length);

			if (compare >= newarray[0]) {
				return findMin(newarray);
			} else {
				newarray[0] = compare;
				return findMin(newarray);
			}
    }
  }
}
