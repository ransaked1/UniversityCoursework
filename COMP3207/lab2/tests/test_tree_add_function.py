import unittest
import requests
import json

# Take note of how we import modules from shared_code 
from shared_code.Tree import Tree, BadCoordinateError, EmptySpeciesError

class TestAddTreeFunction(unittest.TestCase):   
    """
    This test set focuses on testing the responses from the server on the addTree function, separated from tests of the base classes 
    """

    LOCAL_DEV_URL="http://localhost:7071/api/tree/add"
    PUBLIC_URL="https://treehuggers-32161204.azurewebsites.net/api/tree/add?code=m5ptKPf30Zq4ZfP1b_ysGntiOev5wh44duTamTfp4CCSAzFua1qyhg=="
    TEST_URL = PUBLIC_URL
    #Experienced programmers will note the above can be improved to use environment variables. True. Out of scope for this lab 

    # A valid tree  
    tree_1 = Tree(ident=100,
        coordinates=(50.93304715,-1.3900633),address="83 Upper Shaftesbury Avenue, Southampton, SO17 3RW, United Kingdom", species = "Bamboo Bamboozle")
    json_tree_1 = tree_1.to_json()
    
    #invalid trees
    tree_2 = Tree(ident=150,
        coordinates=(513,513),
        address="Somewhere in the Cloud...", 
        species = "Bamboo Bambara")
    
    tree_3 = Tree(ident=180,
                  coordinates=(14,14),
                  address="Highfield",
                  species = "")

    def test_is_valid_correct(self):
       #note we use data instead of params as we want to send json
       #https://requests.readthedocs.io/en/latest/user/quickstart/#more-complicated-post-requests
       response = requests.post(self.TEST_URL,data=self.json_tree_1)
       # the below decodes json that we expect as response
       #https://requests.readthedocs.io/en/latest/user/quickstart/#json-response-content
       dict_response = response.json()     

       self.assertTrue(dict_response['result'])
       self.assertEqual(dict_response['msg'],'OK')
       #TODO: Actually test the DB was correctly updated (Week 4)
        
    def test_is_valid_input(self):
      """
      Activity 2.2: 
      Write a case to test we get the answer {"result": false, "msg": "Input JSON is not from a Tree"} 
      """
      payload = json.dumps({"id": 100, "address": "An address", "species": "Unknown" })
      response = requests.post(self.TEST_URL, data=payload)
      dict_response = response.json()

      self.assertFalse(dict_response['result'])
      self.assertEqual(dict_response['msg'],'Input dict is not from a Tree')
       
    def test_is_valid_coordinates(self):
      """
      Activity 2.2: 
      Write a case to test we get the answer {"result": false, "msg": "Bad Coordinates"} 
      -- Yes, 'msg' should be the message of the exception triggered by is_valid, but we'll let you off the hook as there are many things to do ;) 
      """
      payload1 = json.dumps({"id": 100, "coordinates" : [500, 500] ,"address": "An address", "species": "Unknown" })
      payload2 = json.dumps({"id": 100, "coordinates" : [500.0, 500.0, 500.0] ,"address": "An address", "species": "Unknown" })
      payload3 = json.dumps({"id": 100, "coordinates" : [500.0, 500.0] ,"address": "An address", "species": "Unknown" })
      payload4 = json.dumps({"id": 100, "coordinates" : [0.0, 500.0] ,"address": "An address", "species": "Unknown" })

      response1 = requests.post(self.TEST_URL, data=payload1)
      response2 = requests.post(self.TEST_URL, data=payload2)
      response3 = requests.post(self.TEST_URL, data=payload3)
      response4 = requests.post(self.TEST_URL, data=payload4)

      dict_response1 = response1.json()
      dict_response2 = response2.json()
      dict_response3 = response3.json()
      dict_response4 = response4.json()

      self.assertFalse(dict_response1['result'])
      self.assertFalse(dict_response2['result'])
      self.assertFalse(dict_response3['result'])
      self.assertFalse(dict_response4['result'])

      self.assertTrue(dict_response1['msg'] == 'Coordinates are not floats')
      self.assertTrue(dict_response2['msg'] == 'Coordinates is not a tuple or has wrong length')
      self.assertTrue(dict_response3['msg'] == 'Latitute not in correct range')
      self.assertTrue(dict_response4['msg'] == 'Longitude not in correct range')
