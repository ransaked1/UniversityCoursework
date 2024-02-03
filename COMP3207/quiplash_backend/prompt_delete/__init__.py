import logging
import json
import os
import re

from azure.cosmos import CosmosClient 

from azure.functions import HttpRequest, HttpResponse

# ProxyObjects to account, database and container respectively
MyCosmos = CosmosClient.from_connection_string(os.environ['AzureCosmosDBConnectionString'])
QuiplashDBProxy = MyCosmos.get_database_client(os.environ['DatabaseName'])
PlayerContainerProxy = QuiplashDBProxy.get_container_client(os.environ['PlayerContainerName'])
PromptContainerProxy = QuiplashDBProxy.get_container_client(os.environ['PromptContainerName'])

TranslationKey = os.environ['TranslationKey']
TranslationEndpoint = os.environ['TranslationEndpoint']

SERVER_REGION = "uksouth"
SUPPORTED_LANGUAGES = ['en', 'es', 'it', 'sv', 'ru', 'id', 'bg', 'zh-Hans']

def main(req: HttpRequest) -> HttpResponse:

    req_body = req.get_json()
    logging.info('Buckle up, we are requested to delete prompts from this {}'.format(req_body))

    if 'player' in req_body.keys():
        player = req_body['player']
        counter = 0
        for doc in PromptContainerProxy.read_all_items():
            if doc['username'] == player:
                PromptContainerProxy.delete_item(item = doc, partition_key = doc['username'])
                counter += 1
        return HttpResponse(body = json.dumps({'result': True,'msg' : "{} prompts deleted".format(str(counter))}), status_code = 200, mimetype='application/json')
    
    if 'word' in req_body.keys():
        word = req_body['word']
        counter = 0
        for doc in PromptContainerProxy.read_all_items():
            for text in doc['texts']:
                string = text['text']
                if 'en' == text['language'] and word in re.sub(r'[^\w\s]', '', string).split():
                    PromptContainerProxy.delete_item(item = doc, partition_key = doc['username'])
                    counter += 1
        return HttpResponse(body = json.dumps({'result': True,'msg' : "{} prompts deleted".format(str(counter))}), status_code = 200, mimetype='application/json')
