#We use TinyDB as persistence: https://tinydb.readthedocs.io/en/latest/
from tinydb import TinyDB, Query, where
from tinydb.operations import delete
import requests
import json

class Tree:
  # The ProxyObject to a TinyDB database to persist users
  # Note is a Class variable
  tree_collection = TinyDB("tree_collection.json")  
    
  def __init__(self,ident,coordinates,address,species):
    self.id = ident
    #Tuple of (Lat,Lon)
    self.coordinates = coordinates
    self.address = address
    self.species = species
  
  def __str__(self):
    """
    Output: string representation of Tree
    """
    return """
    id ={}
    coordinates={}
    address={}
    species = {}
    """.format(self.id,self.coordinates,self.address,self.species)
  

  def is_valid(self):
    """
    Validates this tree has the minimum set of attributes to be inserted in DB.
    Returns True if yes, false if not 
    """
    #Activity 1: Validate that attribute 'species'  a non-empty string
    # raise an appropriate ValueError if invalid https://docs.python.org/3/library/exceptions.html#ValueError
    # we raise Exception instead of returning False to provide info on the nature of the error
    if not self.species:
      raise ValueError("Species cannot be empty")

    #Activity 2: Validate that 'coordinates' is a tuple of size 2 and that values are floats that conform to the valid ranges
    # of latitude [-90.0,90.0] and longitude [-180.0,180.0]
    # raise ValueError("Wrong coordinates") if invalid https://docs.python.org/3/library/exceptions.html#ValueError
    # Use the isinstance function for type checking https://www.w3schools.com/python/ref_func_isinstance.asp
    if not isinstance(self.coordinates, tuple) or len(self.coordinates) != 2:
      raise ValueError("Wrong coordinates")
    
    lat, lon = self.coordinates
    if not isinstance(lat, float) or lat < -90.0 or lat > 90.0 or not lat:
      raise ValueError("Wrong coordinates")
    if not isinstance(lon, float) or lon < -180.0 or lon > 180.0 or not lon:
      raise ValueError("Wrong coordinates")

    #Activity 3: Validate that 'address' is a non-empty string
    # raise an appropriate ValueError if invalid https://docs.python.org/3/library/exceptions.html#ValueError
    # we raise Exception instead of returning False to provide info on the nature of the error
    if not self.address:
      raise ValueError("Address cannot be empty")
  
    return True

  @classmethod
  def add_tree(cls,tree):
    """
    Inserts tree into the tree database
    Note: Development frameworks have an Object Relational Mapper (ORM) doing this automagically for you when you create a class. 
    Sometimes you can deploy those in the Cloud. Sometimes you can't (e.g., legacy app), forcing to rethink how you move persistence to the Cloud.
    """

    #Activity 3: Use a try except block to catch the exceptions raised by is_valid()
    # If the exception caught is that 'species' is empty, replace with "Unknown" and continue with insertion
    # If the exception caught is that 'coordinates' is invalid, raise it
    # External resource on Try...except https://www.w3schools.com/python/python_try_except.asp
    # List of Built-in Exceptions: https://docs.python.org/3.10/library/exceptions.html#concrete-exceptions

    try:
      tree.is_valid()
    except ValueError as e:
      if e.args[0] == "Species cannot be empty":
        tree.species = "Unknown"
      else:
        raise e

    # Validate tree is not already in the database
    # TinyDB does not implement integrity constraints! 
    TreeQueryObject = Query()
    if cls.tree_collection.contains(TreeQueryObject.id == tree.id):
      raise ValueError("Attempt to insert a tree with id that already exists")
    
      
    cls.tree_collection.insert({"id" : tree.id,
                      "coordinates" : tree.coordinates,
                      "address" : tree.address ,
                      "species" : tree.species}) 
    return True 


  @classmethod
  def delete_tree(cls,tree):
    """
    Removes tree from the tree database
    """
    #The remove function returns the ids of removed documents
    # https://tinydb.readthedocs.io/en/latest/api.html?highlight=remove#tinydb.table.Table.remove
    removed_ids = cls.tree_collection.remove(where('id') == tree.id)
    
    #We use removed_ids to return an appropriate message
    if len(removed_ids) == 0:
      return "There was no tree with the id you want to remove"
    else:
      return "Tree with id {} deleted".format(removed_ids[0]) 


  def get_address(self):
    """
    Activity 5: Read the documentation of nominatim's reverse geocoder https://nominatim.org/release-docs/develop/api/Reverse/    
    And use it to get the address of this tree from its coordinates.
    You should use the requests library to construct the http request, address must be set to the 'display_name' field of the JSON response
    You may assume the tree is valid.   

    Do take notice of the usage policy https://operations.osmfoundation.org/policies/nominatim/ (1 request per second)
    """
    url = 'https://nominatim.openstreetmap.org/reverse?'

    headers = {
      'User-Agent': 'Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36',
    }

    urlWithParams = url + 'lat={0}&lon={1}&format=json'.format(self.coordinates[0], self.coordinates[1])

    response = requests.get(urlWithParams, headers=headers)

    json = response.json()

    try:
      self.address = json['display_name']
    except KeyError:
      raise ValueError("Could not get address from coordinates")

    # Additional Activity 3 
    # Refactor get_address to raise an exception if nominatim's Reverse can't return an address  
    # You may try coordinates (0,0), in the middle of the ocean, to see what to expect

  def get_coordinates(self):
    """
    Additional Activity 4: Use nominatim's lookup method https://nominatim.org/release-docs/develop/api/Lookup/ 
    # to take this Tree's address and update its coordinates
    """
    pass
    #your code here



  
      
      
    
  

   