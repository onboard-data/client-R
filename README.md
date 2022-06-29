# Onboard API R Client

### General Usage Example 

This example requires an Onboard API key with scopes `general`, `auth` and `buildings:read`

#### Installing API Library and verifying connectivity
```R
install.packages('devtools') # Install devtools package first
devtools::install_github(repo='pranay2811/R-Test',
                          auth='Github-Personal-Access-Token') #Need PAT since repo is currently private
library(OnboardClient)

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

#### Query Data Model

```R
all_equip_types <- get_equip_types() #Query all equipment type in Onboard's Data Model

all_point_types <- get_point_types() #Query all point types in Onboard's Data Model
```

#### Query Building Metadata

```
all_buildings <- get_buildings()  #Query site data for all buildings in your organization

metadata <- get_metadata(buildings) # Query metadata for building id 428

get_metadata(name='Laboratory') #Query metadata for building name: Laboratory
```


### Query Staging Data, deployments and user info

This example requires an Onboard API key with scopes `admin`, `collection:admin`, and `staging` 

```R
get_staged_data(id=428) # Query staged data for building id 428

get_staged_data(name='Laboratory') # Query staged data for building name:Laboratory

get_users() #Qeury all users in your organization

```
