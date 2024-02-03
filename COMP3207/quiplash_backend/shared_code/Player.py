import json

class Player:
    def __init__(self, username, password, games_played, total_score):
        if 4 > len(username) or len(username) > 14:
            raise ValueError("Username less than 4 characters or more than 14 characters")
        if 10 > len(password) or len(password) > 20:
            raise ValueError("Password less than 10 characters or more than 20 characters")
        self.username = username
        self.password = password
        self.games_played = games_played
        self.total_score = total_score

    def to_dict(self):
        return {"username": self.username, "password": self.password, "games_played": self.games_played, "total_score": self.total_score}    

    def to_json(self):
        return json.dumps({"username": self.username, "password": self.password, "games_played": self.games_played, "total_score": self.total_score})    