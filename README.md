# Onboard API R Client

### General Usage Example 

#### Setting Up API Access

If you are an existing Onboard user you can head over to [your account’s api keys page](https://portal.onboarddata.io/account?tab=api) and generate a new key and grant scopes for `general`, `auth` and `buildings:read`.

If you would like to get access to Onboard and start prototyping against an example building please request access [here](https://www.onboarddata.io/contact-us).

#### Installing API Library and verifying connectivity

You can install the official release (stable) from CRAN using the standard `install.packages()` function, or use the development version (unstable) by installing from github. Proceed at your own risk if using the dev version! While the dev version may offer extra functionality, it is our active development platform. This means it is also more prone to error as it's not necessarily fully tested. If something breaks or doesn't work as expected, though, please let us know, as we would certainly like to know so we can push a patch.
```R
# official version (stable)
install.packages('OnboardClient')

# dev version (unstable)
install.packages('devtools') # Install devtools package first
devtools::install_github(repo='onboard-data/client-R') # install from github
```
To test if your API key is working correctly, use `api.setup()`, and enter your api keys in the dialogue box when prompted.
Check the `Remember with keyring` option if you wish to save your api keys securely with the [keyring](https://support.rstudio.com/hc/en-us/articles/360000969634) package.

```R
library(OnboardClient)

api.setup() # set up api url and api keys. Your connection is established if it returns 200.

whoami <- api.get('whoami') # Verify your access to Onboard's API scopes. Generates a list called whoami in R's Global Environment

all_buildings <- get_buildings() #Query site data for all buildings in your organization
```

#### Query Data Model

```R
all_equip_types <- get_equip_types() #Query all equipment types in Onboard's Data Model

all_point_types <- get_point_types() #Query all point types in Onboard's Data Model
```

### Query metadata and timeseries data
```R

query <- PointSelector() # using point selector function

query$buildings <- c(427) 
query$point_types <- c('Supply Air Temperature','Discharge Air Temperature')

selection <- select_points(query) #Select points form database based on your query

#Query points based on the selection
points <- get_points_by_ids(selection$points)

#Query equipment based on the selection
equipment <- get_equipment_by_ids(selection$equipment)


#For clean metadata output
metadata <- get_metadata(selection=selection) #Query metadata by selection list we got above

##OR

metadata <- get_metadata(buildings=c(427,'Laboratory')) # Query entire metadata for building id 427 and building name: Laboratory

#For timeseries output
library(lubridate)

end_time <- as.POSIXlt(Sys.time(), tz = 'UTC')

start_time <- end_time - hours(4)

timeseries <- get_timeseries(start_time= start_time, end_time = end_time, point_ids = selection$points) #Queries timeseries data for the selection list we got above

```

### Query Staging Data, deployments and user info

This example requires an Onboard API key with scopes `admin`, `collection:admin`, and `staging` 

```R
staged_data <- get_staged_data(building = 427) # Query staged data for building id 427

staged_data <- get_staged_data(building ='Laboratory') # Query staged data for building name: Laboratory

deployments <- get_deployments() #Query all deployments in your organization

users <- get_users() #Qeury all users in your organization

```

## License

 Copyright 2018-2022 Onboard Data Inc

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.