from tinydb import TinyDB, Query, where
import json

class Story():
  story_collection = TinyDB("story_collection.json")  
  
  
  def __init__(self,story_id,username,tree_id,text):  
    self.story_id = story_id
    self.username = username
    self.tree_id = tree_id
    self.text = text

  @classmethod
  def add_story(cls,story):
    """
    Input: An instance of a Story
    Output: Returns True if Story successfully persisted in DB
    Raises Value Error otherwise 
    """
    # TinyDB uses QueryObjects to query fields
    Story_QueryObject = Query()
    if not cls.story_collection.contains(Story_QueryObject.id == story.story_id):
      cls.story_collection.insert({"id": story.story_id ,
                                  "username" : story.username ,
                                  "tree_id": story.tree_id, 
                                  "text" : story.text })
      return True
    else:
      return ValueError("Story id {} already exists".format(story.story_id))


  @classmethod
  def huggerboard(cls):
    """
      Output: Dictionary user.username -> number of stories this user has created for any tree formatted as JSON
    """
    huggerboard = {}
    
    # TinyDB ProxyObject implement the iterator pattern
    # meaning we can process it as a list
    for story_json in cls.story_collection:
      # Remember in the collection we have the JSON representations
      uname = story_json['username']
      if uname not in huggerboard.keys():
        huggerboard[uname] = 1
      else:
        huggerboard[uname] = huggerboard[uname] + 1
      
    # Note: tinyDB is very limited! This method has a linear complexity on the length of the list. Won't scale very well, even with the Cloud. 
    # An improvement is the use of a more advanced database that supports aggregation queries 
    return huggerboard

  @classmethod
  def best_friend(cls,tree):
    """
    Activity 6: Implement the following function
    Input: A tree
    Output: The username that has written most stories involving the input Tree and the number of stories.
         formatted as a JSON object of the form {"user": username, "stories" : numstories}
    """
    Story_QueryObject = Query()
    best_friend = cls.story_collection.search(Story_QueryObject.tree_id == tree.id)
    username = best_friend[0]["username"]
    numstories = len(best_friend)
    
    object =  { "user": username, "stories" : numstories }

    return json.dumps(object)


    
