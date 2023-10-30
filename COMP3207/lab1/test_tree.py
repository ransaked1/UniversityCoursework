import unittest
from Tree import Tree

#Read this page on Python's unittest how https://docs.python.org/3.10/library/unittest.html up to section Class Methods
# In section ClassMethods, focus on the setUp() tearDown() and the assert* methods 
class TestTreeMethods(unittest.TestCase):   

    # A valid tree  
    tree_1 = Tree(ident=100,
        coordinates=(50.93304715,-1.3900633),address="85, Upper Shaftesbury Avenue, Portswood, Southampton, England, SO17 3RU, United Kingdom", species = "Bamboo Bamboozle")
    
    #invalid trees
    tree_2 = Tree(ident=150,
        coordinates=(513,513),
        address="Somewhere in the Cloud...", 
        species = "Bamboo Bambara")
    
    tree_3 = Tree(ident=180,
                  coordinates=(14,14),
                  address="Highfield",
                  species = "")


    # Empty the tree collection after each test
    def tearDown(self):
       Tree.tree_collection.truncate()

    #Activity 4: write unit tests for the correct, failed validation of species and failed validation of coordinates cases 
    # of the is_valid() method. For the failed validations, just check a ValueError exception is raised
    # Hint: https://www.pythonclear.com/unittest/python-unittest-assertraises/

    def test_is_valid_correct(self):
       assert self.tree_1.is_valid()
       with self.assertRaises(ValueError):
          self.tree_2.is_valid()
          self.tree_3.is_valid()
    
    def test_is_valid_species(self):
       self.assertIsNotNone(self.tree_1.species)
       self.assertIsNotNone(self.tree_2.species)
       with self.assertRaises(ValueError) as e:
        self.tree_3.is_valid()
        self.assertEqual("Species cannot be empty", str(e.exception))
       
    def test_is_valid_coordinates(self):
       self.assertIsNotNone(self.tree_1.coordinates)
       self.assertIsNotNone(self.tree_2.coordinates)
       with self.assertRaises(ValueError) as e:
        self.tree_2.is_valid()
        self.assertEqual("Wrong coordinates", str(e.exception))
     

    def test_add_tree_valid(self):
      try:
        Tree.add_tree(self.tree_1)
        self.assertEqual(len(Tree.tree_collection),1)
        Tree.add_tree(self.tree_3)
        self.assertEqual(len(Tree.tree_collection),2)
      except:
         assert False

            
    def test_delete_tree_existent(self):
      # Additional activity 1:
      self.assertEqual("There was no tree with the id you want to remove", Tree.delete_tree(self.tree_1))
      Tree.add_tree(self.tree_1)
      self.assertEqual("Tree with id 1 deleted", Tree.delete_tree(self.tree_1))
      self.assertEqual(len(Tree.tree_collection),0)

    def test_get_address(self):
      self.tree_1.get_address()
      self.tree_3.get_address()
      self.assertEqual("85, Upper Shaftesbury Avenue, Portswood, Southampton, England, SO17 3RU, United Kingdom", self.tree_1.address)
      self.assertEqual("Lac البحيرة, Tchad تشاد", self.tree_3.address)

      with self.assertRaises(ValueError):
        self.tree_2.get_address()
      
    

 
      

#Main to execute the tests
if __name__ == '__main__':
    unittest.main()

  
