# Onboard API R Client

### General Usage Example 

This example requires an Onboard API key with scopes `general`, `auth` and `buildings:read`

#### Installing API Library and verifying connectivity
```R
install.packages('devtools') # Install devtools package first
devtools::install_github(repo='onboard-data/client-R',
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

### Query site info for all building in your org
```R
all_buildings <- get_buildings() #Query site data for all buildings in your organization
```

### Query metadata
```R

query <- PointSelector() #using point selector function

query$buildings <- c(427,428) 
query$point_types <- c('Supply Air Temperature','Discharge Air Temperature')

selection <- select_points(query) #Select points form database based on your query

#Query points based on the selection
points <- get_points_by_ids(selection$points)

#Query equipment based on the selection
equipment <- get_equipment_by_ids(selection$equipment)


#For clean metadata output
metadata <- get_metadata(selection=selection) #Query metadata by selection list we got above

##OR

metadata <- get_metadata(buildings=c(427,'Laboratory')) # Query entire metadata for building id 428 and building name: Laboratory

```

### Query Staging Data, deployments and user info

This example requires an Onboard API key with scopes `admin`, `collection:admin`, and `staging` 

```R
staged_data <- get_staged_data(building = 427) # Query staged data for building id 427

staged_data <- get_staged_data(building ='Laboratory') # Query staged data for building name: Laboratory

deployments <- get_deployments() #Query all deployments in your organization

users <- get_users() #Qeury all users in your organization

```

