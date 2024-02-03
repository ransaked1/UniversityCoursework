import json

class Prompt:
    def __init__(self, username, text):
        self.username = username
        self.text = text

    def to_dict(self):
        return {"text": self.text, "username": self.username}    

    def to_json(self):
        return json.dumps({"text": self.text, "username": self.username})