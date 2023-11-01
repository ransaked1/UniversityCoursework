import logging
import os
import json

from azure.functions import HttpRequest, HttpResponse

from azure.cosmos import CosmosClient 
from azure.cosmos.exceptions import CosmosHttpResponseError, CosmosResourceExistsError, CosmosResourceNotFoundError

from ..shared_code import Tree

# ProxyObjects to account, database and container respectively
MyCosmos = CosmosClient.from_connection_string(os.environ['AzureCosmosDBConnectionString'])
TreeHuggersDBProxy = MyCosmos.get_database_client(os.environ['DatabaseName'])
TreeContainerProxy = TreeHuggersDBProxy.get_container_client(os.environ['TreeContainerName'])

def main(req: HttpRequest) -> HttpResponse:

    filter = req.params.get('filter')
    logging.info('Buckle up, we weere asked to filter trees by parameter {}'.format(filter))

    ### TODO: Activity 3
    # Create appropriate ProxyObjects to the Tree collection to implement tree_search function as follows
    # Input: request with query parameter 'filter=XX' with XX a   
    # Output: JSON array representing list of Trees where the Address field includes the string 'XX'
    # For example, if Tree1 has address = "1, Highfield Road, SO17 1BJ, Southampton", 
    # Tree2 has address = "1, Highfield Road, PO17 1BJ, Portsmouth"
    # and filter="Southampton"
    # the output must be a list containing the Tree1 JSON representation.
    # The same tree with filter="Highfield" returns a list with both Tree1 and Tree2
    # Consider the search case-insensitive      
    ###

    try:
        query = "SELECT * FROM t WHERE t.Address CONTAINS '{}'".format(filter)
        query_iterator = TreeContainerProxy.query_items(query, pre_trigger_include=filter, post_trigger_include=filter)
        results = []
        for item in query_iterator:
            results.append(item)
        logging.info("Out binding successfully set")
        return HttpResponse(body=json.dumps(results),mimetype='application/json')
    except CosmosResourceNotFoundError:
        return HttpResponse(body=json.dumps({'result': False,'msg' : "User does not exist"}),mimetype='application/json')
