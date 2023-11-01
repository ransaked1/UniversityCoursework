import unittest
import requests
import json
# Take note of how we import modules from shared_code 
from shared_code.Tree import Tree, BadCoordinateError, EmptySpeciesError
from azure.cosmos import CosmosClient 


class TestAddTreeFunction(unittest.TestCase):   
    """
    This test set focuses on testing the responses from the server on the addTree function, separated from tests of the base classes 
    """
    LOCAL_DEV_URL="http://localhost:7071/tree/add"
    PUBLIC_URL="https://your-deployment-url/api/tree_add"
    TEST_URL = LOCAL_DEV_URL
    # Sadly, tests don't have local.settings.json available as environment variables
    # Hence, we have to parse it manually
    with open('local.settings.json') as settings_file:
        settings = json.load(settings_file)
    MyCosmos = CosmosClient.from_connection_string(settings['Values']['AzureCosmosDBConnectionString'])
    TreeHuggersDBProxy = MyCosmos.get_database_client(settings['Values']['DatabaseName'])
    TreeContainerProxy = TreeHuggersDBProxy.get_container_client(settings['Values']['TreeContainerName'])

    # A valid tree  
    tree_1 = Tree(ident="100",
        coordinates=(50.93304715,-1.3900633),address="85, Upper Shaftesbury Avenue, Portswood, Southampton, England, SO17 3RU, United Kingdom", species = "Bamboo Bamboozle")
    json_tree_1 = tree_1.to_json()
    
    #invalid trees
    tree_2 = Tree(ident="150",
        coordinates=(513,513),
        address="Somewhere in the Cloud...", 
        species = "Bamboo Bambara")
    
    tree_3 = Tree(ident="180",
                  coordinates=(14,14),
                  address="Highfield",
                  species = "")

    def test_is_valid_correct(self):
       #note we use data instead of params as we want to send json
       #https://requests.readthedocs.io/en/latest/user/quickstart/#more-complicated-post-requests
       response = requests.post(self.TEST_URL,params={"code": "Your FunctionAPPkey here"},data=self.json_tree_1)
       # the below decodes json that we expect as response
       #https://requests.readthedocs.io/en/latest/user/quickstart/#json-response-content
       dict_response = response.json()     

       self.assertTrue(dict_response['result'])
       self.assertEqual(dict_response['msg'],'OK')
       #TODO: Activity 1: Complete test to check the right tree was added
          
    def test_is_valid_input(self):
      # Create a fake tree for testing in json format
      fake_tree = json.dumps({"id": 666, "fake" : True})
      response = requests.post(self.TEST_URL,data=fake_tree)
      # Note the response.json() method already desearialises the json string into a Python dictionary  
      self.assertEqual({"result": False, "msg": "Input JSON is not from a Tree"},response.json())
      
       
    def test_is_valid_coordinates(self):      
      bad_coord_tree = {"id":666,"coordinates":(513,513),"address":"Somewhere in the Cloud...", "species" : "Bamboo Bambara"}
      response = requests.post(self.TEST_URL,data=json.dumps(bad_coord_tree))
      # Note the response.json() method already desearialises the json string into a Python dictionary  
      self.assertEqual({"result": False, "msg": "Bad Coordinates"},response.json())    


         
      
