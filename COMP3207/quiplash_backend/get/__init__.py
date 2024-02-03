import logging
import json
import os

from azure.cosmos import CosmosClient 

from azure.functions import HttpRequest, HttpResponse

# ProxyObjects to account, database and container respectively
MyCosmos = CosmosClient.from_connection_string(os.environ['AzureCosmosDBConnectionString'])
QuiplashDBProxy = MyCosmos.get_database_client(os.environ['DatabaseName'])
PromptContainerProxy = QuiplashDBProxy.get_container_client(os.environ['PromptContainerName'])

def main(req: HttpRequest) -> HttpResponse:

    req_body = req.get_json()
    logging.info('Buckle up, we are requested to get from this {}'.format(req_body))

    try:
        result = []
        for prompt_doc in PromptContainerProxy.read_all_items():
            if prompt_doc['username'] in req_body['players']:
                for text in prompt_doc['texts']:
                    if text['language'] == req_body['language']:
                        result.append({"id" : prompt_doc['id'], "text" : text['text'], "username" : prompt_doc['username']})
        result_string = json.dumps(result, sort_keys=True)
        return HttpResponse(body = result_string, status_code=200)
    except:
        return HttpResponse("Something unexpected and bad happened during get, look at the server log", status_code=500)