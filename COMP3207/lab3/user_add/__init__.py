import logging

from azure.functions import HttpRequest, HttpResponse, Out, Document


# Remember, 
# req is the incoming HttpRequest
# doc is the object bound to the out binding
# As this is a outbinding to CosmosDB, user_container_binding implements interface Out (an out binding) 
# Document https://learn.microsoft.com/en-us/python/api/azure-functions/azure.functions.document?view=azure-python#
# As usual, we return a HttpResponse
def main(req: HttpRequest, usercontainerbinding: Out[Document]) -> HttpResponse:

    req_body = req.get_json()
    logging.info('Buckle up, we are requested to add an user from this {}'.format(req_body))

    #Note that as class User use "name", we need to convert that to "id" to match the schema in the container

    try:
        user_doc = {"id": req_body['name'], "city": req_body['city']}
    except:
        return HttpResponse("Something unexpected and bad happened, probably wrong formatted input. Look at it: {}".format(req_body),status_code=500)    
    
    try:
        #Create object of type Azure Document from the request
        # https://learn.microsoft.com/en-us/python/api/azure-functions/azure.functions.document?view=azure-python#azure-functions-document-from-dict
        user_doc_for_cosmos = Document.from_dict(user_doc)
        # user_container_binding is a ProxyObject to the container in the DB
        # the set(method) inserts the document
        usercontainerbinding.set(user_doc_for_cosmos)
        logging.info("Out binding successfully set")
        return HttpResponse("User {} inserted".format(user_doc),status_code=200)
    except Exception as err:
        logging.error(err)
        return HttpResponse("Something unexpected and bad happened during insertion, look at the server log",status_code=500)    

