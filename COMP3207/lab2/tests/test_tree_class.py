import unittest
# Take note of how we import modules from shared_code 
from shared_code.Tree import Tree, BadCoordinateError, EmptySpeciesError


class TestTreeMethods(unittest.TestCase):   

    # A valid tree  
    tree_1 = Tree(ident=100,
        coordinates=(50.93304715,-1.3900633),address="83 Upper Shaftesbury Avenue, Southampton, SO17 3RW, United Kingdom", species = "Bamboo Bamboozle")
    
    #invalid trees
    tree_2 = Tree(ident=150,
        coordinates=(513,513),
        address="Somewhere in the Cloud...", 
        species = "Bamboo Bambara")
    
    tree_3 = Tree(ident=180,
                  coordinates=(14,14),
                  address="Highfield",
                  species = "")
    

    def tearDown(self):
       #No need as we are not using TinyDB anymore
       #Tree.tree_collection.truncate()
       pass


    def test_is_valid_correct(self):
       self.assertTrue(self.tree_1.is_valid())
        
    def test_is_valid_species(self):
      with self.assertRaises(EmptySpeciesError): 
         self.tree_3.is_valid()
       
    def test_is_valid_coordinates(self):
       with self.assertRaises(BadCoordinateError): 
         self.tree_2.is_valid()
    
    """
    TODO: Refactor these when we have a CloudDB (Week 4)
    def test_add_tree_valid(self):
      Tree.add_tree(self.tree_1)
      # Assert result = expected
      self.assertEqual(len(Tree.tree_collection),1)

            
    def test_delete_tree_existent(self):
      Tree.add_tree(self.tree_1)
      Tree.delete_tree(self.tree_1)
      self.assertEqual(len(Tree.tree_collection),0)
    """  

    def test_get_address(self):
      # May want to do this one to test your answer to Activity 5!
      tree = Tree(ident=100,
        coordinates=(50.93304715,-1.3900633),address="83 Upper Shaftesbury Avenue, Southampton, SO17 3RW, United Kingdom", species = "Bamboo Bamboozle")
      tree.get_address()
      self.assertEqual(tree.address,"83 Upper Shaftesbury Avenue, Southampton, SO17 3RW, United Kingdom")        


#Main to execute the tests
if __name__ == '__main__':
    unittest.main()

  
