import logging
import json

from azure.functions import HttpRequest, HttpResponse
from shared_code.Tree import Tree, BadCoordinateError
# To access variables in local.settings.json
# each entry in the dictionary "Values" in local.settings.json is made available as an environment variable when you run func start
# This also works when
import os
# Library to interact with a CosmosDB library
# install it with python -m pip install azure-cosmos in your virtual environment
from azure.cosmos import CosmosClient 
# Exceptions returned by ProxyObjects
# Consult the reference here: https://learn.microsoft.com/en-us/python/api/azure-cosmos/azure.cosmos.exceptions?view=azure-python
from azure.cosmos.exceptions import CosmosHttpResponseError, CosmosResourceExistsError, CosmosResourceNotFoundError

# ProxyObjects to account, database and container respectively
# API reference here
MyCosmos = CosmosClient.from_connection_string(os.environ['AzureCosmosDBConnectionString'])
TreeHuggersDBProxy = MyCosmos.get_database_client(os.environ['DatabaseName'])
TreeContainerProxy = TreeHuggersDBProxy.get_container_client(os.environ['TreeContainerName'])

def main(req: HttpRequest) -> HttpResponse:
    # get_json() decodes the received JSON string into a dict  
    input = req.get_json()
    logging.info('Buckle up, we got a request to add a tree from this JSON: {}'.format(input))
    input_tree = Tree(ident=0)
    try:
        input_tree.from_dict(input)
    except ValueError:
        # we got the exception we implemented in from_dict method
        # return appropriate HttpResponse
        logging.info("Value Error caught from input {}".format(input))
        response_body = json.dumps({"result": False, "msg": "Input JSON is not from a Tree"})
        return HttpResponse(body=response_body,mimetype="application/json")

    try:
        if input_tree.is_valid():
            #Method to create item in a container
            #Read the reference here: https://learn.microsoft.com/en-us/python/api/azure-cosmos/azure.cosmos.containerproxy?view=azure-python#azure-cosmos-containerproxy-create-item 
            #We are not using automatic id generation in this lab, but you will need it for the CW
            TreeContainerProxy.create_item(input_tree.to_dict(),enable_automatic_id_generation=True)
            return HttpResponse(body=json.dumps({"result": True , "msg" : "OK"}),mimetype="application/json")
    except BadCoordinateError:
        # We got BadCoordinate from is_valid method, return appropriate HttpResponse
        # The spec does not distinguish between the two types of BadCoordinate error, so we return the same message
        response_body = json.dumps({"result": False, "msg": "Bad Coordinates"})
        return HttpResponse(body=response_body,mimetype="application/json")
    except CosmosHttpResponseError as cosmoserr:
        #Note from the documentation of create_item that this exception is returned if an item with the same id already exists
        logging.info(cosmoserr)
        response_body = json.dumps(body={"result": False, "msg": "Tree with input id already exist"},mimetype="application/json")
        
