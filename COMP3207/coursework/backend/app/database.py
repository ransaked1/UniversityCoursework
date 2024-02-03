"""
Database operations module.

This module provides functions to interact with the user database.
"""
from typing import Optional

from pymongo import MongoClient
from bson import ObjectId
import gridfs


def documentize(d: dict) -> dict:
    d['_id'] = ObjectId(d.pop('oid'))
    return d


def pythonize(d: dict) -> dict:
    d['oid'] = str(d.pop('_id'))
    return d


class Database:
    # Uses azures key vault
    def __init__(self):
        COSMOS_DB_URI = "mongodb://kissa-db-dpwvhfkg:7i661s7SDxYdbRP3PTGLQEaCUGiaQea0zyqvrNelWu1ZBJByuROpU5D6AbUUcIYZYkl7NaG1IE7VACDbloh7Iw==@kissa-db-dpwvhfkg.mongo.cosmos.azure.com:10255/?ssl=true&replicaSet=globaldb&retrywrites=false&maxIdleTimeMS=120000&appName=@kissa-db-dpwvhfkg@"
        COSMOS_DB_NAME = "kissa-db"
        COSMOS_DB_PROFILE_COLLECTION = "kissa-db-profile-collection"
        COSMOS_DB_MATCH_COLLECTION = "kissa-db-match-collection"
        
        client = MongoClient(COSMOS_DB_URI)
        self.db = client[COSMOS_DB_NAME]
        self.profile_collection = self.db[COSMOS_DB_PROFILE_COLLECTION]
        self.match_collection = self.db[COSMOS_DB_MATCH_COLLECTION]

        self.fs = gridfs.GridFS(self.db)   # initialise gridFS

    def get_user_by_email(self, email: str):
        user = self.profile_collection.find_one({"email": email})
        if user:
            user = pythonize(dict(user))
            return user 
        return None

    def get_user_by_id(self, user_id: str) -> dict | None:
        if not ObjectId.is_valid(user_id):
            print('User id is not valid', user_id)
            return None
        oid = ObjectId(user_id)
        user = self.profile_collection.find_one({'_id': oid})
        if user:
            user = pythonize(dict(user))
            return user
        return None

    def create_user(self, user_dict) -> Optional[str]:
        try:
            user_dict = documentize(user_dict)
            user_dict.pop('_id')
            return str(self.profile_collection.insert_one(user_dict).inserted_id)
        except Exception as e:
            print("Error registering user: " + str(e))
            return None

    def update_user(self, user_id: str, user_dict: dict) -> dict | None:
        try:
            oid = ObjectId(user_id)

            # filter out keys with `None` values
            filtered = {k: v for k, v in user_dict.items() if v is not None}

            update = dict()
            update['$set'] = filtered
            if self.profile_collection.update_one({'_id': oid}, update, upsert=False).acknowledged:
                return self.get_user_by_id(user_id)
            else:
                return None
        except Exception as e:
            print("Error updating user: " + str(e))
            return None

    def get_all_users(self) -> list[dict]:
        all_users = list(self.profile_collection.find())
        return list(map(lambda user: pythonize(user), all_users))

    def delete_user(self, user_id: str) -> bool:
        try:
            oid = ObjectId(user_id)
            self.profile_collection.delete_one({'_id': oid})
            return True
        except Exception as e:
            print("Error deleting user: " + str(e))
            return False

    def upload_file(self, file_data, owner_id):
        # Stores the file in GridFS and is represented as a string id
        # GraphFS stores owner_id using the 'metadata' tag
        file_id = self.fs.put(file_data, metadata={'owner_id': owner_id})
        return file_id
    
    def update_user_profile_pic(self, user_id: str, image_id: str):
        try:
            oid = ObjectId(user_id)
            update = {'$set': {'profile_pic_url': str(image_id)}}
            result = self.profile_collection.update_one({'_id': oid}, update)

            return result.acknowledged
        except Exception as e:
            print("Error updating user profile picture: " + str(e))
            return None
    
    def update_cat_profile_images(self, user_id: str, image_id: str):
        try:
            user = self.get_user_by_id(user_id)
            if user and 'cat' in user:
                cat_images = user['cat'].get('image_ids', [])
                cat_images.append(str(image_id))
                update = {'$set': {'cat.image_ids': cat_images}}
                result = self.profile_collection.update_one({'_id': user_id}, update)
                return result.acknowledged
            return False
        except Exception as e:
            print("Error updating cat profile images: " + str(e))
            return None

    def get_file(self, _id):
        try:
            file_id = ObjectId(_id)
            file = self.fs.get(file_id)

            return file
        except gridfs.errors.NoFile:
            return None

    def delete_file(self, _id):
        try:
            # Remove image from database using its id
            file_id = ObjectId(_id)
            self.fs.delete(file_id)

            # Remove entry in profile
            self.profile_collection.update_many(
                {"cat.image_ids": str(file_id)},
                {"$pull": {"cat.image_ids": str(file_id)}}
            )

            return True
        except gridfs.errors.NoFile:
            return False 
        except Exception as e:
            print("Error when deleting file: " + str(e))
            return False

    def create_match(self, match_dict: dict) -> str | None:
        try:
            match_dict = documentize(match_dict)
            match_dict.pop('_id')
            return str(self.match_collection.insert_one(match_dict).inserted_id)
        except Exception as e:
            print("Error creating match: " + str(e))
            return None

    def get_match(self, match_id: str) -> dict | None:
        if not ObjectId.is_valid(match_id):
            print('Match id is not valid', match_id)
            return None
        oid = ObjectId(match_id)
        match = self.match_collection.find_one({'_id': oid})
        if match:
            match = pythonize(dict(match))
            return match
        return None

    def add_messages(self, messages: list[dict], match_id: str) -> bool:
        match = self.get_match(match_id)
        match['messages'].append(messages)
        try:
            oid = ObjectId(match_id)

            update = dict()
            update['$set'] = {'messages': match['messages']}
            if self.match_collection.update_one({'_id': oid}, update, upsert=False).acknowledged:
                return True
            else:
                return False
        except Exception as e:
            print("Error adding messages: " + str(e))
            return False

    def confirm_meeting(self, user_id: str, match_id: str) -> list[str] | None:
        match = self.get_match(match_id)
        confirmations = match['meeting_confirmation']
        if user_id in confirmations:
            return confirmations
        try:
            oid = ObjectId(match_id)

            update = dict()
            update['$set'] = {'meeting_confirmation': confirmations}
            if self.match_collection.update_one({'_id': oid}, update, upsert=False).acknowledged:

                return self.get_match(match_id)['meeting_confirmation']
            else:
                return None
        except Exception as e:
            print("Error adding messages: " + str(e))
            return None

    # Only use for testing purposes
    def clear_database(self):
        print('Clearing database...')
        print(f"Profiles entries: {len(list(self.profile_collection.find()))}")
        print(f"Match entries: {len(list(self.match_collection.find()))}")
        print(f"Files entries: {len(list(self.fs.find()))}")

        for entry in self.profile_collection.find():
            self.profile_collection.delete_one(entry)

        for entry in self.match_collection.find():
            self.match_collection.delete_one(entry)

        # for file in self.fs.find():
        #     self.fs.delete(file._id)

        print('Database cleared.')
        print(f"Profiles entries: {len(list(self.profile_collection.find()))}")
        print(f"Match entries: {len(list(self.match_collection.find()))}")
        print(f"Files entries: {len(list(self.fs.find()))}")


if __name__ == "__main__":
    db = Database()
    db.clear_database()
