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
    logging.info('Buckle up, we are requested to update a player from this {}'.format(req_body))
    
    try:
        query = 'SELECT * FROM player r WHERE r.id="{}"'.format(req_body['username'])
        query_iterator = PlayerContainerProxy.query_items(query = query, enable_cross_partition_query = True)

        results = []
        for item in query_iterator:
            results.append(item)

        if len(results) == 0:
            return HttpResponse(body = json.dumps({'result': False, 'msg' : "Player does not exist"}), status_code = 404, mimetype='application/json')
        
        read_item = PlayerContainerProxy.read_item(item = req_body['username'], partition_key = req_body['username'])
        read_item['games_played'] = read_item['games_played'] + req_body['add_to_games_played']
        read_item['total_score'] = read_item['total_score'] + req_body['add_to_score']
        response = PlayerContainerProxy.replace_item(item = read_item, body = read_item)

        logging.info('Replaced {} with new value: {} and {}'.format(response['id'], read_item['games_played'], read_item['total_score']))
        return HttpResponse(body = json.dumps({"result" : True, "msg": "OK" }), status_code=200)
    except Exception:
        return HttpResponse("Something unexpected and bad happened during player updating, look at the server log",status_code=500)
