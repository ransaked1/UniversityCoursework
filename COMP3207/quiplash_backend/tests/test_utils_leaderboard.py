import unittest
import requests
import json

from shared_code.Player import Player
from azure.cosmos import CosmosClient

class TestUtils(unittest.TestCase):   
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

    LOCAL_DEV_URL_UPDATE = "http://localhost:7071/player/update"
    PUBLIC_URL_UPDATE = DeploymentURL + "/player/update?code=" + FunctionAppKey
    TEST_URL_UPDATE = PUBLIC_URL_UPDATE

    LOCAL_DEV_URL = "http://localhost:7071/utils/leaderboard"
    PUBLIC_URL = DeploymentURL + "/utils/leaderboard?code=" + FunctionAppKey
    TEST_URL = PUBLIC_URL

    def test_utils_leaderboard_empty(self):
        response = requests.get(self.TEST_URL, data = json.dumps({"top" : 0}))

        self.assertEqual(200, response.status_code)

        response_json = json.loads(response.text)
        self.assertEqual(0, len(response_json))

    def test_utils_leaderboard_no_tiebreaks(self):
        self.setup_no_tiebreaks()

        response = requests.get(self.TEST_URL, data = json.dumps({"top" : 5}))

        self.assertEqual(200, response.status_code)

        response_json = json.loads(response.text)

        self.assertEqual(5, len(response_json))
        self.assertEqual('X-player', response_json[0]['username'])
        self.assertEqual('D-player', response_json[1]['username'])
        self.assertEqual('C-player', response_json[2]['username'])
        self.assertEqual('A-player', response_json[3]['username'])
        self.assertEqual('B-player', response_json[4]['username'])

    def test_utils_leaderboard_games_played_tiebreaks(self):
        self.setup_games_played_tiebreaks()

        response = requests.get(self.TEST_URL, data = json.dumps({"top" : 5}))

        self.assertEqual(200, response.status_code)

        response_json = json.loads(response.text)

        self.assertEqual(5, len(response_json))
        self.assertEqual('X-player', response_json[0]['username'])
        self.assertEqual('D-player', response_json[1]['username'])
        self.assertEqual('C-player', response_json[2]['username'])
        self.assertEqual('A-player', response_json[3]['username'])
        self.assertEqual('B-player', response_json[4]['username'])

    def test_utils_leaderboard_both_tiebreaks(self):
        self.setup_both_tiebreaks()

        response = requests.get(self.TEST_URL, data = json.dumps({"top" : 5}))

        self.assertEqual(200, response.status_code)

        response_json = json.loads(response.text)

        self.assertEqual(5, len(response_json))
        self.assertEqual('X-player', response_json[0]['username'])
        self.assertEqual('D-player', response_json[1]['username'])
        self.assertEqual('C-player', response_json[2]['username'])
        self.assertEqual('A-player', response_json[3]['username'])
        self.assertEqual('B-player', response_json[4]['username'])

    def setup_no_tiebreaks(self) -> None:
        player = Player(username = "A-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "B-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "C-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "D-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "X-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "Y-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "Z-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())

        update = {"username": "A-player", "add_to_games_played": 10, "add_to_score" : 60 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "B-player", "add_to_games_played": 10, "add_to_score" : 50 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "C-player", "add_to_games_played": 20, "add_to_score" : 70 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "D-player", "add_to_games_played": 10, "add_to_score" : 80 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "X-player", "add_to_games_played": 50, "add_to_score" : 100 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "Y-player", "add_to_games_played": 10, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "Z-player", "add_to_games_played": 1, "add_to_score" : 10 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))

    def setup_games_played_tiebreaks(self) -> None:
        player = Player(username = "A-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "B-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "C-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "D-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "X-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "Y-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "Z-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())

        update = {"username": "A-player", "add_to_games_played": 20, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "B-player", "add_to_games_played": 30, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "C-player", "add_to_games_played": 10, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "D-player", "add_to_games_played": 5, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "X-player", "add_to_games_played": 1, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "Y-player", "add_to_games_played": 40, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "Z-player", "add_to_games_played": 50, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))

    def setup_both_tiebreaks(self) -> None:
        player = Player(username = "A-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "B-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "C-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "D-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "X-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "Y-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())
        player = Player(username = "Z-player", password = "password1234", games_played = 0, total_score = 0)
        requests.post(self.TEST_URL_REGISTER, data = player.to_json())

        update = {"username": "A-player", "add_to_games_played": 10, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "B-player", "add_to_games_played": 10, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "C-player", "add_to_games_played": 20, "add_to_score" : 80 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "D-player", "add_to_games_played": 10, "add_to_score" : 80 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "X-player", "add_to_games_played": 50, "add_to_score" : 100 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "Y-player", "add_to_games_played": 10, "add_to_score" : 40 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))
        update = {"username": "Z-player", "add_to_games_played": 1, "add_to_score" : 10 }
        requests.put(self.TEST_URL_UPDATE, data = json.dumps(update))

    def tearDown(self) -> None:
        for doc in self.PlayerContainerProxy.read_all_items():
            self.PlayerContainerProxy.delete_item(item = doc, partition_key = doc['id'])