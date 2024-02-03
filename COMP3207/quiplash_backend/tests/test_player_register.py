import unittest
import requests
import json

from shared_code.Player import Player

from azure.cosmos import CosmosClient


class TestPlayerRegister(unittest.TestCase):
    with open('local.settings.json') as settings_file:
        settings = json.load(settings_file)
        
    MyCosmos = CosmosClient.from_connection_string(settings['Values']['AzureCosmosDBConnectionString'])
    QuiplashDBProxy = MyCosmos.get_database_client(settings['Values']['DatabaseName'])
    PlayerContainerProxy = QuiplashDBProxy.get_container_client(settings['Values']['PlayerContainerName'])

    DeploymentURL = settings['Values']['DeploymentURL']
    FunctionAppKey = settings['Values']['FunctionAppKey']

    LOCAL_DEV_URL = "http://localhost:7071/player/register"
    PUBLIC_URL = DeploymentURL + "/player/register?code=" + FunctionAppKey
    TEST_URL = PUBLIC_URL


    def test_player_register_valid(self):
        valid_player = {"username": "ransaked", "password": "password1234", "games_played": 0, "total_score": 0}
        response = requests.post(self.TEST_URL, data = json.dumps(valid_player))

        self.assertEqual(200, response.status_code)
        self.assertEqual('{"result": true, "msg": "OK"}', response.text)

        query_result = self.PlayerContainerProxy.read_item("ransaked", partition_key="ransaked")
        query_result_stripped = {"username": query_result['id'], "password": query_result['password'], 
                                 "games_played": query_result['games_played'], "total_score": query_result['total_score']}
        
        self.assertEqual(json.dumps(query_result_stripped), json.dumps(valid_player))

    def test_player_register_exists(self):
        valid_player = {"username": "ransaked", "password": "password4321", "games_played": 0, "total_score": 0}
        duplicate_player = {"username": "ransaked", "password": "password41", "games_played": 0, "total_score": 0}

        response1 = requests.post(self.TEST_URL, data = json.dumps(valid_player))
        response2 = requests.post(self.TEST_URL, data = json.dumps(duplicate_player))

        self.assertEqual(200, response1.status_code)
        self.assertEqual(409, response2.status_code)

        self.assertEqual('{"result": true, "msg": "OK"}', response1.text)
        self.assertEqual('{"result": false, "msg": "Username already exists"}', response2.text)

    def test_player_register_invalid(self):
        invalid_username1 = {"username": "ran", "password": "password1234", "games_played": 0, "total_score": 0}
        invalid_username2 = {"username": "123456789012345", "password": "password1234", "games_played": 0, "total_score": 0}
        invalid_password1 = {"username": "ransaked", "password": "pass1234", "games_played": 0, "total_score": 0}
        invalid_password2 = {"username": "ransaked", "password": "123456789012345678901", "games_played": 0, "total_score": 0}

        response1 = requests.post(self.TEST_URL, data = json.dumps(invalid_username1))
        response2 = requests.post(self.TEST_URL, data = json.dumps(invalid_username2))
        response3 = requests.post(self.TEST_URL, data = json.dumps(invalid_password1))
        response4 = requests.post(self.TEST_URL, data = json.dumps(invalid_password2))

        self.assertEqual(400, response1.status_code)
        self.assertEqual(400, response2.status_code)
        self.assertEqual(400, response3.status_code)
        self.assertEqual(400, response4.status_code)

        self.assertEqual('{"result": false, "msg": "Username less than 4 characters or more than 14 characters"}', response1.text)
        self.assertEqual('{"result": false, "msg": "Username less than 4 characters or more than 14 characters"}', response2.text)
        self.assertEqual('{"result": false, "msg": "Password less than 10 characters or more than 20 characters"}', response3.text)
        self.assertEqual('{"result": false, "msg": "Password less than 10 characters or more than 20 characters"}', response4.text)


    def tearDown(self) -> None:
        for doc in self.PlayerContainerProxy.read_all_items():
            self.PlayerContainerProxy.delete_item(item=doc, partition_key=doc['id'])