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
  
  def to_dict(self):
    dict_representation = {'id':self.id, 'coordinates': self.coordinates, 'address': self.address, 'species': self.species }
    return dict_representation

  
  def to_json(self):
    """
    Activity 2.1: Implement a function that takes this tree and returns the json string serialisation of a dictionary that represents it:
    {"id": the_id, "coordinates": the_coordinates , "address": the_address, "species": the_species}
    Validate the tree and re-raise any exception from the is_valid() method
    """
    try:
      self.is_valid()
    except ValueError:
      raise
   
    dict_representation = self.to_dict()
    return json.dumps(dict_representation)
    
  def from_dict(self,dict_tree):
    """
    Activity 2.1: Implement a function that takes a dictionary and replace the attributes of this tree with its values.
    Raise a ValueError("Input dict is not from a Tree") if the input dict_tree does not have exactly the four keys that define a tree (empty values are allowed)
    """
    if set(dict_tree.keys()) != {'id','coordinates','address','species'}:
      raise ValueError("Input dict is not from a Tree")
    
    self.id = dict_tree['id']
    self.coordinates = dict_tree['coordinates']
    self.address = dict_tree['address']
    self.species = dict_tree['species']

  



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
    
    if not (-180.0 <= self.coordinates[0] < 180.0):
      raise BadCoordinateError("Longitude not in correct range")  
    
    return True

  #TODO: Activity 4, refactor this method to use Azure Maps instead of Nominatim
  def get_address(self):
    """ Takes the coordinates of this tree and sets the address based on the response of Nominatim's web servce
    """

    # The old nominatim's code
    #payload = {"lat" : self.coordinates[0] , "lon": self.coordinates[1], "format": "jsonv2"}
    #r = requests.get("https://nominatim.openstreetmap.org/reverse",params=payload)
    #json_resp = r.json()

    #https://learn.microsoft.com/en-us/rest/api/maps/search/get-search-address-reverse?tabs=HTTP
    azure_maps_url = "https://atlas.microsoft.com/search/address/reverse/json"
    
    #Note the use of the format method to construct the https://www.w3schools.com/python/ref_string_format.asp
    parameters = {"api-version" : "1.0", "subscription-key" : "Your subscription key" ,
                  "query":"{lat},{lon}".format(lat=self.coordinates[0],lon=self.coordinates[1])}

    response = requests.get(azure_maps_url,params=parameters)
    # The address from Azure is inside a listof possible addresses, we take the first one.      
    address_from_azure = response.json()['addresses'][0]['address']   
    print(address_from_azure)
    #Construct the one line address from the dictionary of address components given by Azure Maps
    one_line_address = "{number}, {road}, {city}, {nation}, {postcode}, {country}"
    one_line_address = one_line_address.format(number=address_from_azure['streetNumber'],
                                               road=address_from_azure['streetName'],
                                               # Note we have to sacrifice the ward, as Azure Maps does not have the information                           
                                               #ward=address_from_azure['countrySubdivisionName'],
                                               city=address_from_azure['municipality'],
                                               nation=address_from_azure['countrySubdivisionName'],
                                               postcode=address_from_azure['extendedPostalCode'],
                                               country=address_from_azure['country'])    
    self.address=one_line_address

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





  
      
      
    
  

   