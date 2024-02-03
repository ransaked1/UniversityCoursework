import unittest
import requests
import json

from shared_code.Player import Player
from azure.cosmos import CosmosClient


class TestPlayerUpdate(unittest.TestCase):
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

    LOCAL_DEV_URL = "http://localhost:7071/player/update"
    PUBLIC_URL = DeploymentURL + "/player/update?code=" + FunctionAppKey
    TEST_URL = PUBLIC_URL

    def test_player_update_valid(self):
        player_expected = Player(username = "ransaked", password = "password1234", games_played = 2, total_score = 211)

        valid_update1 = {"username": "ransaked", "password": "pwd", "add_to_games_played": 1, "add_to_score" : 100 }
        valid_update2 = {"username": "ransaked", "add_to_games_played": 1, "add_to_score" : 111 }
        response1 = requests.put(self.TEST_URL, data = json.dumps(valid_update1))
        response2 = requests.put(self.TEST_URL, data = json.dumps(valid_update2))

        self.assertEqual('{"result": true, "msg": "OK"}', response1.text)
        self.assertEqual('{"result": true, "msg": "OK"}', response2.text)

        query_result = self.PlayerContainerProxy.read_item("ransaked", partition_key="ransaked")
        query_result_stripped = {"username": query_result['id'], "password": query_result['password'], 
                                 "games_played": 2, "total_score": 211}
        
        self.assertEqual(query_result_stripped, player_expected.to_dict())

    def test_player_update_invalid(self):
        non_existent_player = {"username": "ransa", "password": "pwd", "add_to_games_played": 1, "add_to_score" : 100 }
        response = requests.put(self.TEST_URL, data = json.dumps(non_existent_player))

        self.assertEqual('{"result": false, "msg": "Player does not exist"}', response.text)

    def setUp(self) -> None:
        valid_player = Player(username = "ransaked", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = valid_player.to_json())

    def tearDown(self) -> None:
        for doc in self.PlayerContainerProxy.read_all_items():
            self.PlayerContainerProxy.delete_item(item=doc, partition_key=doc['id'])