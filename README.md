# OnboardClient: API Bindings for R

### General Usage Example 

#### Setting Up API Access

If you are an existing Onboard user you can head over to [your account’s api keys page](https://portal.onboarddata.io/account?tab=api) and generate a new key and grant scopes for `general`, `auth` and `buildings:read`.

If you would like to get access to Onboard and start prototyping against an example building please request access [here](https://www.onboarddata.io/sandbox).

Instructions and examples for how to use these API bindings are further documented on [our docs!](https://onboard-api-wrappers-documentation.readthedocs.io/en/latest/index.html)

#### Installing API Library and verifying connectivity

You can install OnboardClient from github. 

```R
install.packages('devtools') # Install devtools package first
devtools::install_github(repo='onboard-data/client-R') # install from github
```
Connect with the API using `api.setup()`, and enter your api keys in the dialogue box when prompted.
Check the `Remember with keyring` option if you wish to save your api keys securely with the [keyring](https://support.rstudio.com/hc/en-us/articles/360000969634) package.

```R
library(OnboardClient)

api.setup() # set up api url and api keys. If your connection is established, it returns a welcome message.

whoami <- api.request('whoami') # Verify your access to Onboard's API scopes. Generates a list called whoami in R's Global Environment

all_buildings <- api.request("buildings") #Query building information for all buildings in your organization

building_search <- search_buildings(buildings="lab") #Search building information for specific buildings using search string (non case-sensitive) 

org_search <- search_orgs(orgs = "onboard") #Search specific organization if you access to more than one orgs (non case-sensitive)
```

#### Query Data Model

```R
all_equip_types <- api.request("equiptype") #Query all equipment types in Onboard's Data Model

all_point_types <- api.request("pointtypes") #Query all point types in Onboard's Data Model
# OR
all_point_types <- get_point_types() #Query a clean version of point_types
```

### Query metadata and timeseries data
```R

query <- PointSelector() # using point selector function

query$buildings <- c(427) 
query$point_types <- c('supply_air_temeperature_sensor','zone_air_temperature_sensor')

selection <- select_points(query) #Select points form database based on your query

#Query points based on the selection
points <- get_points_by_ids(selection$points)

#Query equipment based on the selection
equipment <- get_equipment_by_ids(selection$equipment)


#For clean metadata output
metadata <- get_metadata(
  buildings = 427,
  point_types = c(
    'supply_air_temeperature_sensor',
    'zone_air_temperature_sensor')
)

##OR

metadata <- get_metadata(orgs = "onboard",buildings=c(427,'Laboratory')) # Query entire metadata for building id 427 and building name: Laboratory inside org name: onboard

#For timeseries output
library(lubridate)

end_time <- as.POSIXlt(Sys.time(), tz = 'UTC')

start_time <- end_time - hours(4)

timeseries <- get_timeseries(start_time= start_time, end_time = end_time, point_ids = selection$points) #Queries timeseries data for the selection list we got above

```

### Query Staging Data, deployments and user info

This example requires an Onboard API key with scopes `admin`, `collection:admin`, and `staging` 

```R
staged_data <- get_staging_data(buildings = c(427,"Laboratory") # Query staged data for building id 427 & building name: Laboratory

deployments <- get_deployments() #Query all deployments in your organization

users <- get_users() #Qeury all users in your organization

```

## License

 Copyright 2018-2026 Onboard Data Inc

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
