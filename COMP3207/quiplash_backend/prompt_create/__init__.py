import logging
import json
import os
import uuid
import requests
import copy

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
    logging.info('Buckle up, we are requested to create a prompt from this {}'.format(req_body))

    try:
        query = 'SELECT * FROM player r WHERE r.id="{}"'.format(req_body['username'])
        query_iterator = PlayerContainerProxy.query_items(query = query, enable_cross_partition_query = True)

        results = []
        for item in query_iterator:
            results.append(item)

        if len(results) == 0:
            return HttpResponse(body = json.dumps({'result': False, 'msg' : "Player does not exist"}), status_code = 404, mimetype='application/json')
        
        text = req_body['text']

        if len(text) > 80 or len(text) < 15:
            return HttpResponse(body = json.dumps({'result': False,'msg' : "Prompt less than 15 characters or more than 80 characters"}), status_code = 409, mimetype='application/json')
    except Exception as err:
        return HttpResponse("Something unexpected and bad happened during prompt validation, look at the server log", status_code=500)
    
    try:
        path = '/detect?api-version=3.0'
        constructed_url = TranslationEndpoint + path

        headers = {
            'Ocp-Apim-Subscription-Key': TranslationKey,
            'Ocp-Apim-Subscription-Region': SERVER_REGION,
            'Content-type': 'application/json',
            'X-ClientTraceId': str(uuid.uuid4())
        }

        body = [{'text': req_body['text']}]

        request = requests.post(constructed_url, headers=headers, json=body)
        response = request.json()

        language = response[0]['language']
        score = response[0]['score']

        logging.info(language + " " + str(score))

        if language not in SUPPORTED_LANGUAGES or score < 0.3:
            return HttpResponse(body = json.dumps({'result': False, 'msg' : "Unsupported language"}), status_code = 406, mimetype='application/json')
    except Exception as err:
        return HttpResponse("Something unexpected and bad happened during prompt text language detection, look at the server log", status_code=500)

    try:
        path = '/translate'
        constructed_url = TranslationEndpoint + path

        languages = copy.deepcopy(SUPPORTED_LANGUAGES)
        languages.remove(language)

        params = {
            'api-version': '3.0',
            'from': language,
            'to': languages
        }

        request = requests.post(constructed_url, params=params, headers=headers, json=body)
        response = request.json()

        texts = []
        texts.append({"language" : language, "text" : req_body['text']})
        for field in response[0]['translations']:
            texts.append({"language" : field['to'], "text" : field['text']})

        logging.info(json.dumps(texts, sort_keys=True, ensure_ascii=False, indent=4, separators=(',', ': ')))
    except Exception as err:
        return HttpResponse("Something unexpected and bad happened during prompt text doc creation, look at the server log", status_code=500)

    try:
        prompt_doc = {"username": req_body['username'], "texts": texts}
        logging.info(prompt_doc)

        response = PromptContainerProxy.upsert_item(body=prompt_doc)
        return HttpResponse(body = json.dumps({"result" : True, "msg": "OK" }), status_code=200)
    except Exception as err:
        return HttpResponse("Something unexpected and bad happened during prompt creation, look at the server log", status_code=500)
