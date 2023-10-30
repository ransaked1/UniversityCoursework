import unittest
from Story import Story
from tinydb import TinyDB, Query
from datetime import date
import json
# Needed to test best_friend method
import Tree

class TestStoryMethods(unittest.TestCase):

  #Mock stories for testing that we can reuse across all tests

  story1 = Story(story_id="10",username="Bob",tree_id=100,text="Hello Tree")
  story2 = Story(story_id="11",username="Bob",tree_id=110,text="Hello again, Tree")
  story3 = Story(story_id="12",username="Alice",tree_id=110,text="Howdy, Tree")
   
  
  def test_add_story(self):
    self.fail("TODO")

  def test_huggerboard(self):
    #We insert all our mock trees to create a data state
    # where we know the answer Bob is the top hugger
    Story.add_story(self.story1)
    Story.add_story(self.story2)
    Story.add_story(self.story3)
  
    huggerboard = Story.huggerboard()
    # Must have one key,value in my dictionary for all huggers in the Database
    self.assertEqual(len(huggerboard), 2)
    #and values match the answer we know correct
    self.assertEqual(huggerboard['Bob'],2)
    self.assertEqual(huggerboard['Alice'],1)

  def test_best_friend(self):
    #We insert all our mock trees
    Story.add_story(self.story1)
    Story.add_story(self.story2)
    Story.add_story(self.story3)
    # for s in Story.story_collection:
    #   print(json.dumps(s))

    #In that database state, Bob is best friend of tree_id=100
    # First, we create a mock Tree with tree_id = 100
    mock_tree = Tree.Tree(ident=100,coordinates=(30,20),address="Highfield Campus",species="Pinus Mocksus")
    bf_100 = Story.best_friend(mock_tree)
    # assert we get the json corresponding to the known answer
    self.assertEqual(bf_100,json.dumps({"user": "Bob", "stories": 1}))

    # This test reminded us we did not specify what happens in case of ties! 

    # Additional activity 2: 
    # Refactor best_friend to, in case of many users having the same number of stories attached to a tree return a JSON list instead of a single one.
    # Write an unittest for that case.
  #def test_best_friends_multiple(self):  


  def setUp(self):
    #Clear story_collection before each test
    #An improvement is to refactor Class Story to a different TinyDB collection, then we set that to a test_collection
    Story.story_collection.truncate()

  def tearDown(self):
    #Clear story_collection after each test
    #An improvement is to refactor Class Story to configure a different TinyDB collection, then we set that to a test_collection
    Story.story_collection.truncate()
      

#Main to execute the tests
if __name__ == '__main__':
    unittest.main()

  
