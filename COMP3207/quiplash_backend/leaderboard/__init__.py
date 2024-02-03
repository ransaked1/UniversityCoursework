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
    logging.info('Buckle up, we are requested to make a leaderboard from this {}'.format(req_body))
    
    try:
        query = 'SELECT TOP {} * FROM player ORDER BY player.total_score DESC, player.games_played ASC, player.id ASC'.format(req_body['top'])
        query_iterator = PlayerContainerProxy.query_items(query = query, enable_cross_partition_query = True)

        result = []
        for item in query_iterator:
            result.append({"username": item['id'], "games_played": item['games_played'], "total_score": item['total_score']})

        result_string = json.dumps(result, sort_keys=False)

        logging.info(result)
        return HttpResponse(body = result_string, status_code = 200, mimetype='application/json')
    except Exception:
        return HttpResponse("Something unexpected and bad happened during leaderboard query, look at the server log", status_code=500)