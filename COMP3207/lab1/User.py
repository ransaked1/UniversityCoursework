from tinydb import TinyDB, Query

class User:
    # The ProxyObject to a TinyDB database to persist users
    # Note is a Class variable
    user_collection = TinyDB("user_collection.json")

    def __init__(self, username):
        self.username = username

    def __str__(self):
        return self.username

    # This external resource explains the classmethod decorator
    # https://sentry.io/answers/difference-between-staticmethod-and-classmethod-function-decorators-in-python/
    @classmethod
    def add_user(cls, user):
        """
         Add user to the user database
        """
        # Doc on the basic usage of TinyDB https://tinydb.readthedocs.io/en/latest/getting-started.html#basic-usage
        # Doc on Queries and Query objects https://tinydb.readthedocs.io/en/latest/usage.html#queries
        UserQueryProxy = Query()
        if cls.user_collection.contains(UserQueryProxy.id == user.username):
            raise ValueError(
                "Attempt to insert a tree with id that already exists")

        cls.user_collection.insert({"id": user.username})
