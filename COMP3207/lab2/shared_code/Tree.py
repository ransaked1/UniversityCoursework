#We cannot use TinyDB anymore =( - we'll get into the reasons on week 4
#from tinydb import TinyDB, Query, where
#from tinydb.operations import delete
import requests
import json

class BadCoordinateError(ValueError):
  pass

class EmptySpeciesError(ValueError):    
    pass    

class Tree:
  # The ProxyObject to a TinyDB database to persist users
  # Note is a Class variable
  #TODO: Refactor to a ProxyObject to a Cloud Database (Week 4) 
  #tree_collection = TinyDB("tree_collection.json")  

  #We added default values to some parameters to facilitate Tree creation   
  def __init__(self,ident,coordinates=(-500,-500),address="",species=""):
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
  
  def to_json(self):
    """
    Activity 2.1: Implement a function that takes this tree and returns the json string serialisation of a dictionary that represents it:
    {"id": the_id, "coordinates": the_coordinates , "address": the_address, "species": the_species}
    Validate the tree and re-raise any exception from the is_valid() method
    """
    try:
      self.is_valid()
      return json.dumps({"id": self.id, "coordinates": self.coordinates, "address": self.address, "species": self.species })
    except (BadCoordinateError,EmptySpeciesError) as e:
      raise e
    
  def from_dict(self,dict_tree):
    """
    Activity 2.1: Implement a function that takes a dictionary and replace the attributes of this tree with its values.
    Raise a ValueError("Input dict is not from a Tree") if the input dict_tree does not have exactly the four keys that define a tree (empty values are allowed)
    """
    try:
      self.id = dict_tree['id']
      self.coordinates = dict_tree['coordinates']
      self.address = dict_tree['address']
      self.species = dict_tree['species']
    except KeyError:
      raise ValueError("Input dict is not from a Tree")


  



  def is_valid(self):
    """
    Validates this tree has the minimum set of attributes to be inserted in DB.
    Returns True if yes, raise exception appropriate exception if not 
    """

    if self.species == '':
      raise EmptySpeciesError("Species is empty")

    if not (isinstance(self.coordinates,tuple) or len(self.coordinates) == 2):
      raise BadCoordinateError("Coordinates is not a tuple or has wrong length")  
    
    if not (isinstance(self.coordinates[1],float) or isinstance(self.coordinates[1],float)):
      raise BadCoordinateError("Coordinates are not floats")  
    
    if not (-90.0 <= self.coordinates[0] < 90.0):
      raise BadCoordinateError("Latitute not in correct range")  
    
    if not (-180.0 <= self.coordinates[1] < 180.0):
      raise BadCoordinateError("Longitude not in correct range")  
    
    return True

  #TODO: Activity 4, refactor this method to use Azure Maps instead of Nominatim
  def get_address(self):
    """ Takes the coordinates of this tree and sets the address based on the response of Azure Maps web service
    """
    request = "https://atlas.microsoft.com/search/address/reverse/json?api-version=1.0&subscription-key=8KbSHq-betETr6_CLw4faX-eoz1mK_VjMzy-1VEStTM&query=" + str(self.coordinates[0]) + "," + str(self.coordinates[1])
    r = requests.get(request)
    json_resp = r.json()
    
    #self.address = json_resp['display_name']

    # Additional Activity 3 
    # Refactor get_address to raise an exception if nominatim's Reverse can't return an address  
    # You may try coordinates (0,0), in the middle of the ocean, to see what to expect
    
    # The address field exists even in the middle of the ocean, but there is no countyr

    if not ('address' in json_resp['addresses'][0].keys()):
      raise ValueError("No address detected")

    self.address = json_resp['addresses'][0]['address']['freeformAddress'] + ", " + json_resp['addresses'][0]['address']['country']


  """
  TODO: Refactor this to use Cloud Database (Week 4) 
  @classmethod
  def add_tree(cls,tree):
    
    try:
      tree.is_valid()
    except BadCoordinateError as valerr:
      raise valerr
    except EmptySpeciesError as valerr:      
        tree.species = "Unknown" 
       
    TreeQueryObject = Query()
    if cls.tree_collection.contains(TreeQueryObject.id == tree.id):
      raise ValueError("Attempt to insert a tree with id that already exists")
    
      
    cls.tree_collection.insert({"id" : tree.id,
                      "coordinates" : tree.coordinates,
                      "address" : tree.address ,
                      "species" : tree.species})
    return True  


  TODO: Refactor this to use Cloud Database (Week 4) 
  @classmethod
  def delete_tree(cls,tree):
    
    #The remove function returns the ids of removed documents
    # https://tinydb.readthedocs.io/en/latest/api.html?highlight=remove#tinydb.table.Table.remove
    removed_ids = cls.tree_collection.remove(where('id') == tree.id)
    
    #We use removed_ids to return an appropriate message
    if len(removed_ids) == 0:
      return "There was no tree with the id you want to remove"
    else:
      return "Tree with id {} deleted".format(removed_ids[0]) 

  """    





  def get_coordinates(self):
    """
    Additional Activity 4: Use nominatim's Lookup method https://nominatim.org/release-docs/develop/api/Lookup/ 
    #### CORRECTION: Should be the search method https://nominatim.org/release-docs/develop/api/Search/ 
    # to take this Tree's address and update its coordinates
    """
    payload = {"q" : self.address, "format" : "jsonv2"}
    r = requests.get("https://nominatim.openstreetmap.org/search",params=payload)
    # The output here might be a list of places, we take the first
    json_resp = r.json()[0]
    self.coordinates = (float(json_resp['lat']),float(json_resp['lon']))




  
      
      
    
  

   