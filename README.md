# Onboard API R Library

### General Usage Example 

This example requires an Onboard API key with scopes `general`, `auth` and `buildings:read`

#### Setting up API and verifying connectivity
```R
source("API Library.R")

api.setup() 
# This sets up the api url and api keys in the R environment. 

api.setup(api_type='dev')
# This sets up the dev api url and api keys in the R environment.  

```
When you run `api.setup()`, enter your api keys in the dialogue box when prompted.
Check the `Remember with keyring` option if you wish to save your api keys securely with the [keyring](https://support.rstudio.com/hc/en-us/articles/360000969634) package  


```R
api.status() #Verify you connection with the API. Your connection is established if it returns 200

api.get('whoami') #Verify your access to Onboard's API scopes. Generates a list called whoami in R's Global Environment
```

#### Query Data Model & Metadata

```R
get_equip_types() #Query all equipment type in Onboard's Data Model

get_point_types() #Query all point types in Onboard's Data Model

api.get('buildings') #Query site data for all buildings in your organization

get_metadata(id=428) # Query metadata for building id 428

get_metadata(name='Laboratory') #Query metadata for building name: Laboratory
```


### Query Staging Data, deployments and user info

This example requires an Onboard API key with scopes `admin`, `collection:admin`, and `staging` 

```R
get_staged_data(id=428) # Query staged data for building id 428

get_staged_data(name='Laboratory') # Query staged data for building name:Laboratory

get_users() #Qeury all users in your organization

```
