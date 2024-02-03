import unittest
import requests
import json

from shared_code.Prompt import Prompt
from shared_code.Player import Player
from azure.cosmos import CosmosClient

class TestUtilsGet(unittest.TestCase):   
    with open('local.settings.json') as settings_file:
        settings = json.load(settings_file)

    MyCosmos = CosmosClient.from_connection_string(settings['Values']['AzureCosmosDBConnectionString'])
    QuiplashDBProxy = MyCosmos.get_database_client(settings['Values']['DatabaseName'])
    PlayerContainerProxy = QuiplashDBProxy.get_container_client(settings['Values']['PlayerContainerName'])
    PromptContainerProxy = QuiplashDBProxy.get_container_client(settings['Values']['PromptContainerName'])

    DeploymentURL = settings['Values']['DeploymentURL']
    FunctionAppKey = settings['Values']['FunctionAppKey']

    LOCAL_DEV_URL_REGISTER = "http://localhost:7071/player/register"
    PUBLIC_URL_REGISTER = DeploymentURL + "/player/register?code=" + FunctionAppKey
    TEST_URL_REGISTER = PUBLIC_URL_REGISTER

    LOCAL_DEV_URL_CREATE = "http://localhost:7071/prompt/create"
    PUBLIC_URL_CREATE = DeploymentURL + "/prompt/create?code=" + FunctionAppKey
    TEST_URL_CREATE = PUBLIC_URL_CREATE

    LOCAL_DEV_URL = "http://localhost:7071/utils/get"
    PUBLIC_URL = DeploymentURL + "/utils/get?code=" + FunctionAppKey
    TEST_URL = PUBLIC_URL

    def test_utils_get_not_empty(self):
        response = requests.get(self.TEST_URL, data = json.dumps({"players" : ["ransaked", "ransa"], "language" : "en"}))
        response1 = requests.get(self.TEST_URL, data = json.dumps({"players" : ["ransaked", "ransa"], "language" : "ru"}))
        response2 = requests.get(self.TEST_URL, data = json.dumps({"players" : ["ransaked"], "language" : "es"}))
        response3 = requests.get(self.TEST_URL, data = json.dumps({"players" : ["ransaked", "not_there"], "language" : "zh-Hans"}))

        self.assertEqual(200, response.status_code)
        self.assertEqual(200, response1.status_code)
        self.assertEqual(200, response2.status_code)
        self.assertEqual(200, response3.status_code)

        self.assertEqual(2, len(json.loads(response.text)))
        self.assertEqual(2, len(json.loads(response1.text)))
        self.assertEqual(1, len(json.loads(response2.text)))
        self.assertEqual(1, len(json.loads(response3.text)))

    def test_utils_get_empty(self):
        response = requests.get(self.TEST_URL, data = json.dumps({"players" : [], "language" : "en"}))
        response1 = requests.get(self.TEST_URL, data = json.dumps({"players" : ["not_there"], "language" : "ru"}))
        response2 = requests.get(self.TEST_URL, data = json.dumps({"players" : ["ransaked"], "language" : "ro"}))
        response3 = requests.get(self.TEST_URL, data = json.dumps({"players" : ["ransaked", "not_there"], "language" : "zh"}))

        self.assertEqual(200, response.status_code)
        self.assertEqual(200, response1.status_code)
        self.assertEqual(200, response2.status_code)
        self.assertEqual(200, response3.status_code)

        self.assertEqual(0, len(json.loads(response.text)))
        self.assertEqual(0, len(json.loads(response1.text)))
        self.assertEqual(0, len(json.loads(response2.text)))
        self.assertEqual(0, len(json.loads(response3.text)))

    def setUp(self) -> None:
        player = Player(username = "ransaked", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "ransa", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "another", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())

        prompt = Prompt(text = "Where is the library?", username = "ransaked")
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        prompt = Prompt(text = "Where is the lib", username = "ransa")
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        prompt = Prompt(text = "Where is the library?", username = "another")
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())

    def tearDown(self) -> None:
        for doc in self.PlayerContainerProxy.read_all_items():
            self.PlayerContainerProxy.delete_item(item = doc, partition_key = doc['id'])
        for doc in self.PromptContainerProxy.read_all_items():
            self.PromptContainerProxy.delete_item(item = doc, partition_key = doc['username'])