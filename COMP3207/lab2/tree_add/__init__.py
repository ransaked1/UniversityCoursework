import logging
import json

from azure.functions import HttpRequest, HttpResponse
#Note how we import things from shared_code
from shared_code.Tree import BadCoordinateError, EmptySpeciesError, Tree


def main(req: HttpRequest) -> HttpResponse:
    """
    Activity 2.3: Refactor the code below to conform to the following specification 
    Input: A request containing a JSON allegedly representing a Tree
    Output: one of the following Responses in JSON
    {"result": true, "msg": OK} if input Tree valid and inserted in DB or if input Tree has Species empty, in which case is set to "Unknown" and inserted 
    {"result": false, "msg": "Input JSON is not from a Tree"} if input does not conform  
    {"result": false, "msg": "Bad Coordinates"} if input Tree does not have valid coordinates
    Assume the request contains valid JSON 
    We don't know how to insert in DB yet, so focus on returning the right messages    
    """
    # get_json() function already JSON string in the request into a Python dict  
    input = req.get_json()
    logging.info('Buckle up, we got a request to add a tree from this JSON: {}'.format(input))
    input_tree = Tree(ident=0)

    try:
        input_tree.from_dict(input)
        input_tree.is_valid()
        #TODO: Insert in DB (Week 4)
        return HttpResponse(body=json.dumps({"result": True , "msg" : "OK"}),mimetype="application/json")
    except Exception as e:
        return HttpResponse(body=json.dumps({"result": False , "msg" : str(e)}),mimetype="application/json")
    
    # You may want to experiment what happens when you don't return a HttpResponse
    # It's worthy to identify the bug "forgot to return a HttpResponse" or "forgot to catch an exception in my flow"

