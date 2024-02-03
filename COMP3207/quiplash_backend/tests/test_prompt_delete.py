import unittest
import requests
import json

from shared_code.Prompt import Prompt
from shared_code.Player import Player
from azure.cosmos import CosmosClient

class TestPromptDelete(unittest.TestCase):   
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

    LOCAL_DEV_URL = "http://localhost:7071/prompt/delete"
    PUBLIC_URL = DeploymentURL + "/prompt/delete?code=" + FunctionAppKey
    TEST_URL = PUBLIC_URL

    def test_prompt_delete(self):
        response = requests.post(self.TEST_URL, data = json.dumps({"player" : "ransaked"}))
        response1 = requests.post(self.TEST_URL, data = json.dumps({"player" : "none"}))
        response2 = requests.post(self.TEST_URL, data = json.dumps({"word" : "lib"}))
        response3 = requests.post(self.TEST_URL, data = json.dumps({"word" : "not_there"}))
        response4 = requests.post(self.TEST_URL, data = json.dumps({"word" : "Dimana"}))

        self.assertEqual(200, response.status_code)
        self.assertEqual(200, response1.status_code)
        self.assertEqual(200, response2.status_code)
        self.assertEqual(200, response3.status_code)
        self.assertEqual(200, response4.status_code)

        self.assertEqual('{"result": true, "msg": "3 prompts deleted"}', response.text)
        self.assertEqual('{"result": true, "msg": "0 prompts deleted"}', response1.text)
        self.assertEqual('{"result": true, "msg": "3 prompts deleted"}', response2.text)
        self.assertEqual('{"result": true, "msg": "0 prompts deleted"}', response3.text)
        self.assertEqual('{"result": true, "msg": "0 prompts deleted"}', response4.text)

    def setUp(self) -> None:
        player = Player(username = "ransaked", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "ransa", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "another", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())

        prompt = Prompt(text = "Where is the library?", username = "ransaked")
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        prompt = Prompt(text = "Where is the lib", username = "ransa")
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        prompt = Prompt(text = "Where is the library?", username = "another")
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())
        requests.post(self.TEST_URL_CREATE, data = prompt.to_json())

    def tearDown(self) -> None:
        for doc in self.PlayerContainerProxy.read_all_items():
            self.PlayerContainerProxy.delete_item(item = doc, partition_key = doc['id'])
        for doc in self.PromptContainerProxy.read_all_items():
            self.PromptContainerProxy.delete_item(item = doc, partition_key = doc['username'])