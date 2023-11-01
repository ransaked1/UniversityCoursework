import json

class User:
    def __init__(self,name,city):
        if len(name) < 3:
            raise ValueError("name must be at least 3 characters")
        self.name = name
        self.city = city

    def to_dict(self):
        return {"name": self.name, "city": self.city}    

    def to_json(self):
        return json.dumps({"name": self.name, "city": self.city})    