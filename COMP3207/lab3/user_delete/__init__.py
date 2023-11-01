import logging

from azure.functions import HttpRequest, HttpResponse

import os
import json

from azure.cosmos import CosmosClient
from azure.cosmos.exceptions import CosmosResourceNotFoundError

MyCosmos = CosmosClient.from_connection_string(os.environ['AzureCosmosDBConnectionString'])
TreeHuggersDBProxy = MyCosmos.get_database_client(os.environ['DatabaseName'])
UserContainerProxy = TreeHuggersDBProxy.get_container_client(os.environ['UserContainerName'])


def main(req: HttpRequest) -> HttpResponse:
    name = req.params.get('name')
    logging.info('Buckle up, we got a request to delete a user with this name : {}'.format(name))

    ### TODO: Activity 2
    # Create an appropriate ProxyObject to the User collection and use it to implement delete user as follows
    # Input: Request with query parameter "name=XX" with XX the username to delete (No JSON input this time!)
    # Output (Stil JSON this time!):
    # {'result': true , 'msg': "User deleted"} if user deleted succesfully
    # {'result': false, 'msg' : "User does not exist"} if user asked to delete does not exist in the DB  
    # Hint: You need to read and understand method delete_item https://learn.microsoft.com/en-us/python/api/azure-cosmos/azure.cosmos.containerproxy?view=azure-python#azure-cosmos-containerproxy-delete-item
    # Hint2: the pre_trigger_include and post_trigger_include parameters have default values = None that you can leave. This means you just need to understand how to use of the id and partition_key parameters  
    # 
    ### 

    try:
        UserContainerProxy.delete_item(id = name , partition_key = name)
        logging.info("Out binding successfully set")
        return HttpResponse(body=json.dumps({'result': True , 'msg': "User deleted"}),mimetype='application/json')
    except CosmosResourceNotFoundError:
        return HttpResponse(body=json.dumps({'result': False, 'msg' : "User does not exist"}),mimetype='application/json')