import logging
import json
import os

from azure.cosmos import CosmosClient 
from azure.functions import HttpRequest, HttpResponse, Out, Document

from shared_code.Player import Player

# ProxyObjects to account, database and container respectively
MyCosmos = CosmosClient.from_connection_string(os.environ['AzureCosmosDBConnectionString'])
QuiplashDBProxy = MyCosmos.get_database_client(os.environ['DatabaseName'])
PlayerContainerProxy = QuiplashDBProxy.get_container_client(os.environ['PlayerContainerName'])

def main(req: HttpRequest, usercontainerbinding: Out[Document]) -> HttpResponse:

    req_body = req.get_json()
    logging.info('Buckle up, we are requested to register a player from this {}'.format(req_body))

    try:
        player_doc = {"id": req_body['username'], "password": req_body['password'], "games_played": 0, "total_score": 0}
    except:
        return HttpResponse("Something unexpected and bad happened, probably wrong formatted input. Look at it: {}".format(req_body),status_code=500)
    
    try:
        Player(username = req_body['username'], password = req_body['password'], games_played = 0, total_score = 0)
    except ValueError as err:
        msg = str(err)
        return HttpResponse(body = json.dumps({"result": False, "msg": msg}), status_code = 400)
    
    try:
        query = 'SELECT * FROM player r WHERE r.id="{}"'.format(req_body['username'])
        query_iterator = PlayerContainerProxy.query_items(query = query, enable_cross_partition_query = True)

        results = []
        for item in query_iterator:
            results.append(item)

        if len(results) == 1:
            return HttpResponse(body = json.dumps({'result': False, 'msg' : "Username already exists"}), status_code = 409, mimetype='application/json')

        player_doc_for_cosmos = Document.from_dict(player_doc)
        usercontainerbinding.set(player_doc_for_cosmos)

        return HttpResponse(body = json.dumps({"result" : True, "msg": "OK" }), status_code=200)
    except Exception as err:
        return HttpResponse("Something unexpected and bad happened during player registration, look at the server log",status_code=500)
