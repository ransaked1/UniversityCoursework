# kissa

## Important links

1. API URL: [https://kissa-api.jollymoss-4112728e.uksouth.azurecontainerapps.io/](https://kissa-api.jollymoss-4112728e.uksouth.azurecontainerapps.io/)
2. API docs: [https://kissa-api.jollymoss-4112728e.uksouth.azurecontainerapps.io/docs](https://kissa-api.jollymoss-4112728e.uksouth.azurecontainerapps.io/docs)
3. React app: [https://kissa-web.jollymoss-4112728e.uksouth.azurecontainerapps.io/](https://kissa-web.jollymoss-4112728e.uksouth.azurecontainerapps.io/)

## Prerequisites

1. Install Docker runtime for your platform [here](https://docs.docker.com/engine/install/).

These steps are only needed only if deploying infrastructure:

2. Install Terraform from [here](https://developer.hashicorp.com/terraform/install).

3. Install the Azure CLI following [this guide](https://learn.microsoft.com/en-us/cli/azure/install-azure-cli).

4. Login in the Azure CLI with
```
az login
```

## Running locally

1. Navigate to backend folder and run:
```
docker compose up --build
```

3. Navigate to frontend folder and run:
```
docker compose up --build
```

## Deploying Azure infrastructure with Terraform

Make sure you are logged into the Azure CLI.

Navigate to backend folder and run:
```
terraform init
terraform apply
```

However, since the both frontend and backend are docker containerised.
It can be deployed on the fresh Azure cloud service.
Make sure you also set the environmental variables for the backend:
```
SECRET_KEY

COSMOS_DB_URI

COSMOS_DB_NAME

COSMOS_DB_PROFILE_COLLECTION

COSMOS_DB_MATCH_COLLECTION
```