import unittest
import requests
import json

from shared_code.Player import Player
from azure.cosmos import CosmosClient


class TestPlayerLogin(unittest.TestCase):
    with open('local.settings.json') as settings_file:
        settings = json.load(settings_file)

    MyCosmos = CosmosClient.from_connection_string(settings['Values']['AzureCosmosDBConnectionString'])
    QuiplashDBProxy = MyCosmos.get_database_client(settings['Values']['DatabaseName'])
    PlayerContainerProxy = QuiplashDBProxy.get_container_client(settings['Values']['PlayerContainerName'])

    DeploymentURL = settings['Values']['DeploymentURL']
    FunctionAppKey = settings['Values']['FunctionAppKey']

    LOCAL_DEV_URL_REGISTER = "http://localhost:7071/player/register"
    PUBLIC_URL_REGISTER = DeploymentURL + "/player/register?code=" + FunctionAppKey
    TEST_URL_REGISTER = PUBLIC_URL_REGISTER

    LOCAL_DEV_URL = "http://localhost:7071/player/login"
    PUBLIC_URL = DeploymentURL + "/player/login?code=" + FunctionAppKey
    TEST_URL = PUBLIC_URL

    def test_player_login_valid(self):
        valid_login = {"username": "ransaked", "password": "password1234"}
        response = requests.get(self.TEST_URL, data = json.dumps(valid_login))

        self.assertEqual('{"result": true, "msg": "OK"}', response.text)

    def test_player_login_invalid(self):
        invalid_username = {"username": "ransa", "password": "password1234"}
        invalid_password = {"username": "ransaked", "password": "Password1234"}
        invalid_both = {"username": "ransa", "password": "password"}

        response1 = requests.get(self.TEST_URL, data = json.dumps(invalid_username))
        response2 = requests.get(self.TEST_URL, data = json.dumps(invalid_password))
        response3 = requests.get(self.TEST_URL, data = json.dumps(invalid_both))

        self.assertEqual('{"result": false, "msg": "Username or password incorrect"}', response1.text)
        self.assertEqual('{"result": false, "msg": "Username or password incorrect"}', response2.text)
        self.assertEqual('{"result": false, "msg": "Username or password incorrect"}', response3.text)

    def setUp(self) -> None:
        valid_player = Player(username = "ransaked", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = valid_player.to_json())

    def tearDown(self) -> None:
        for doc in self.PlayerContainerProxy.read_all_items():
            self.PlayerContainerProxy.delete_item(item=doc, partition_key=doc['id'])