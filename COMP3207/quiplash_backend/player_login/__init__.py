import logging
import json
import os

from azure.cosmos import CosmosClient 

from azure.functions import HttpRequest, HttpResponse

# ProxyObjects to account, database and container respectively
MyCosmos = CosmosClient.from_connection_string(os.environ['AzureCosmosDBConnectionString'])
QuiplashDBProxy = MyCosmos.get_database_client(os.environ['DatabaseName'])
PlayerContainerProxy = QuiplashDBProxy.get_container_client(os.environ['PlayerContainerName'])

def main(req: HttpRequest) -> HttpResponse:

    req_body = req.get_json()
    logging.info('Buckle up, we are requested to login player from this {}'.format(req_body))
    
    try:
        query = 'SELECT * FROM player r WHERE r.id="{}" AND r.password="{}"'.format(req_body['username'], req_body['password'])
        query_iterator = PlayerContainerProxy.query_items(query = query, enable_cross_partition_query = True)

        result = []
        for item in query_iterator:
            result.append(item)

        if len(result) == 1:
            return HttpResponse(body = json.dumps({'result': True, 'msg' : "OK"}), status_code = 200, mimetype='application/json')
        else:
            return HttpResponse(body = json.dumps({'result': False, 'msg' : "Username or password incorrect"}), status_code = 401, mimetype='application/json')
    except Exception as err:
        return HttpResponse("Something unexpected and bad happened during login, look at the server log", status_code=500)
