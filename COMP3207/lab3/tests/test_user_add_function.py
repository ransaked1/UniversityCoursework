import unittest
import requests
import json
from shared_code.User import User
from azure.cosmos.exceptions import CosmosHttpResponseError, CosmosResourceExistsError, CosmosResourceNotFoundError
from azure.cosmos import CosmosClient


class TestAddUserFunction(unittest.TestCase):   
    LOCAL_DEV_URL="http://localhost:7071/user/add"
    PUBLIC_URL="https://treehuggers-ldig1y14-2324.azurewebsites.net/user/add"
    TEST_URL = LOCAL_DEV_URL
    # Sadly, tests don't have local.settings.json available as environment variables
    # Hence, we have to parse it manually
    # Note you have to run tests from the project folder and not from the test fodler, otherwise the 
    with open('local.settings.json') as settings_file:
        settings = json.load(settings_file)
    MyCosmos = CosmosClient.from_connection_string(settings['Values']['AzureCosmosDBConnectionString'])
    TreeHuggersDBProxy = MyCosmos.get_database_client(settings['Values']['DatabaseName'])
    UserContainerProxy = TreeHuggersDBProxy.get_container_client(settings['Values']['UserContainerName'])


    def test_user_add_valid(self):
        valid_user = User(name="Ilika",city="Cardiff")
        response = requests.post(self.TEST_URL,data=valid_user.to_json())
        # For brevity, just check the response code
        self.assertEqual(200,response.status_code)
        #Now we do the check directly in the DB (Similar how it will be done for marking ;)
        query_result = self.UserContainerProxy.read_item("Ilika",partition_key="Ilika")
        #Remember documents in DB include Cosmos metadata, we need to strip that for comparison
        # we also store the name as id 
        query_result_stripped = {"name": query_result['id'], "city": query_result['city']}
        self.assertEqual(query_result_stripped,valid_user.to_dict())


    def tearDown(self) -> None:
       # TODO: Additional activity
       # Use the read_all_items() method of ContainerProxy to delete all items in the container
       for doc in self.UserContainerProxy.read_all_items():
          self.UserContainerProxy.delete_item(item=doc,partition_key=doc['id'])


