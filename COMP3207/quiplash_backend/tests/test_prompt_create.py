import unittest
import requests
import json

from shared_code.Prompt import Prompt
from shared_code.Player import Player
from azure.cosmos import CosmosClient

class TestPromptCreate(unittest.TestCase):   
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

    LOCAL_DEV_URL = "http://localhost:7071/prompt/create"
    PUBLIC_URL = DeploymentURL + "/prompt/create?code=" + FunctionAppKey
    TEST_URL = PUBLIC_URL

    def test_prompt_create_valid(self):
        valid_prompt = Prompt(text = "Where is the library?", username = "ransaked")
        response = requests.post(self.TEST_URL, data = valid_prompt.to_json())

        self.assertEqual(200, response.status_code)
        self.assertEqual('{"result": true, "msg": "OK"}', response.text)

    def test_prompt_invalid(self):
        nonexistent_username_prompt = Prompt(text = "Where is the library?", username = "ransa")
        wrong_length_prompt1 = Prompt(text = "Hello", username = "ransaked")
        wrong_length_prompt2 = Prompt(text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. \
                                      Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", username = "ransaked")

        response1 = requests.post(self.TEST_URL, data = nonexistent_username_prompt.to_json())
        response2 = requests.post(self.TEST_URL, data = wrong_length_prompt1.to_json())
        response3 = requests.post(self.TEST_URL, data = wrong_length_prompt2.to_json())

        self.assertEqual(404, response1.status_code)
        self.assertEqual(409, response2.status_code)
        self.assertEqual(409, response3.status_code)

        self.assertEqual('{"result": false, "msg": "Player does not exist"}', response1.text)
        self.assertEqual('{"result": false, "msg": "Prompt less than 15 characters or more than 80 characters"}', response2.text)
        self.assertEqual('{"result": false, "msg": "Prompt less than 15 characters or more than 80 characters"}', response3.text)

    def test_prompt_unsupported(self):
        number_prompt = Prompt(text = "123456789123456789", username = "ransaked")
        garbled_prompt = Prompt(text = "γ̴̟̚o̴̙͛ɿ̶͇͋ ̴͈̾ƨ̷̢͠ɒ̷͓̋b̵̀ͅ ̷̰́*̶̬̔*̶͖́ ̴̈͜ɔ̴̗̐ɒ̴̢͐i̴̹͊n̵̗̚ɘ̵̖̒", username = "ransaked")
        unsuppported_prompt = Prompt(text = "Unde este biblioteca?", username = "ransaked")

        response1 = requests.post(self.TEST_URL, data = number_prompt.to_json())
        response2 = requests.post(self.TEST_URL, data = garbled_prompt.to_json())
        response3 = requests.post(self.TEST_URL, data = unsuppported_prompt.to_json())

        self.assertEqual(406, response1.status_code)
        self.assertEqual(406, response2.status_code)
        self.assertEqual(406, response3.status_code)

        self.assertEqual('{"result": false, "msg": "Unsupported language"}', response1.text)
        self.assertEqual('{"result": false, "msg": "Unsupported language"}', response2.text)
        self.assertEqual('{"result": false, "msg": "Unsupported language"}', response3.text)

    def setUp(self) -> None:
        valid_player = Player(username = "ransaked", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = valid_player.to_json())

    def tearDown(self) -> None:
        for doc in self.PlayerContainerProxy.read_all_items():
            self.PlayerContainerProxy.delete_item(item = doc, partition_key = doc['id'])
        for doc in self.PromptContainerProxy.read_all_items():
            self.PromptContainerProxy.delete_item(item = doc, partition_key = doc['username'])