import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.*;
import uk.ac.soton.ecs.comp1206.labtestlibrary.datastructure.Tree;

public class MinTree implements MinimumInTree {

  public int findMin(Tree tree) {
    if (tree == null) return 0;
    else {
      int left, right;
      int min = tree.getVal();

      if (tree.left() != null) {
        left = findMin(tree.left());
        min = Math.min(min, left);
      }

      if (tree.right() != null) {
        right = findMin(tree.right());
        min = Math.min(min, right);
      }

      return min;
    }
  }
}
