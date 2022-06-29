# Import Libraries --------------------------------------------------------
library(tidyverse)
library(plyr)
library(httr)
library(jsonlite)
library(keyring)
library(lubridate)
library(anytime)
library(rrapply)
library(data.table)
options(stringsAsFactors = F)


# Set API keys and URL ----------------------------------------------------------

api.setup <- function(api_type) {
  
  if(missing(api_type)) {
    api_type <-'prod'
    api_url <- 'https://api.onboarddata.io'

    rstudioapi::askForSecret(
      name='api_key_prod',
      message = 'Enter your API key here',
      title="Onboard API Keys")

    api_key <- keyring::key_get('RStudio Keyring Secrets',
                       username='api_key_prod')

  } else if (api_type == 'dev') {
    api_url <- 'https://devapi.onboarddata.io'

    rstudioapi::askForSecret(
      name = 'api_key_dev',
      message='Enter your DEV API key here',
      title = "Onboard API Keys")

    api_key <- keyring::key_get('RStudio Keyring Secrets',
                       username='api_key_dev')

  }

assign('api_url', api_url, envir = parent.frame())

assign('api_key', api_key, envir = parent.frame())

#key_list()
# api_key_r <- key_get('RStudio Keyring Secrets',username = 'api_key')


}

api.status <- function() {

  request <- GET(url = api_url,
                 add_headers(`X-OB-Api` = api_key))

  print(paste0("API Status: ", request$status_code))


}


# Basic API Query with endpoint -------------------------------------------

## Get API
api.get <- function(endpoint){
  # get endpoint
  endpoint_url <- paste(api_url, endpoint, sep = '/')

  request_endpoint <- GET(url = endpoint_url,
                          content_type_json(),
                          add_headers(`X-OB-Api` = api_key))

  if (request_endpoint$status_code == 200) {
    api_output <-
      content(request_endpoint, as = 'text', encoding = 'UTF-8') %>%
      fromJSON(flatten = T)

    return(api_output)

  } else{

    stop(paste0('API Status Code: ', request_endpoint$status_code))

  }
}

## POST API
api.post <- function(endpoint,json_body){

   # post endpoint
  endpoint_url <- paste(api_url, endpoint, sep = '/')

  request_endpoint <- POST(url = endpoint_url,
                          content_type_json(),
                          add_headers(`X-OB-Api` = api_key),
                          body=json_body)

  if (request_endpoint$status_code == 200) {
    api_output <-
      content(request_endpoint, as = 'text', encoding = 'UTF-8') %>%
      fromJSON(flatten = T)

    return(api_output)

  } else{

    stop(paste0('API Status Code: ', request_endpoint$status_code))

  }

}

# Retrieve Databases -------------------------------------------------------

##Orgs
get_orgs <- function(id){
  orgs <- api.get('organizations')

  orgs <- orgs$data

  if (!missing(id)) {

    id = as.integer(id)

    id = as.data.frame(id) %>%
      filter(id!='')

    if(nrow(id)>=1){
      orgs <- semi_join(orgs, id,
                        by = 'id')
    }
  }
  return(orgs)
}

##Buildings
get_buildings <-function(id){
  buildings <- api.get('buildings')

  if(!missing(id)){

    id = as.integer(id)
    id = as.data.frame(id) %>%
      filter(id!='')

    if(nrow(id)>=1){

    buildings <- semi_join(buildings,id,
                      by='id')
    }
  }
    return(buildings)
  }


## Get User info
get_users <- function(id){

  #Get roles db
  roles <- api.get('roles')

  roles <- roles$data %>%
    select(id,role=name)

  #Get user db
  users <- api.get('users')

  #Format users

  users <- users$data %>%
    select(id,org_id,org_name,roles,email,username,first_name,last_name,last_login,created,password_reset,active) %>%
    mutate_at(vars(password_reset, last_login, created ),
              ~ as_datetime(as.numeric(substr(., 1, 10))),
              tz = 'America/New_York') %>%
    mutate_at(vars(roles),
              ~gsub('c\\(|\\)','',.)) %>%
    mutate_at(vars(roles),
              ~gsub(':',', ',.)) %>%
    separate(col = 'roles',
             into=c('role1','role2','role3','role4'),
             sep=', ',fill = 'right') %>%
    pivot_longer(cols = c(4:7),values_to ='role_id') %>%
    filter(!is.na(role_id)) %>%
    mutate_at(vars(role_id),~as.integer(.))  %>%
    left_join(roles,
              by=c('role_id'='id')) %>%
    select(id,org_id,org_name,role,email,username,first_name,last_name,last_login,created,password_reset,active)

  if(!missing(id)){
    id=as.integer(id)
    id=as.data.frame(id)

    if(nrow(id)>=1){
      users <- semi_join(users,id,
                         by='id')

    }

  }

  return(users)
}

## Get Deployment Stats
get_deployments <- function(org_id){

  deployments <- api.get('deployment')

  deployments <- deployments %>%
    mutate_at(vars(last_heartbeat),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York')) %>%
    select(-api_key,-wg_pubkey)

  if(!missing(org_id)){
    org_id = as.integer(org_id)
    org_id = as.data.frame(org_id)

    if(nrow(org_id)>=1){
      deployments <-semi_join(deployments,org_id,
                              by='org_id')
    }
  }

  return(deployments)

}


# Delete Endpoint ---------------------------------------------------------

##Delete points/equipment from live data. Use with caution
api.delete <- function(building,entity,data_to_delete){

  get_building_info(building)

  endpoints <- paste('buildings',id,entity,sep='/')

  for (i in 1:nrow(data_to_delete)) {

    single_id <- data_to_delete$id[i]

    endpoint_url <- paste(api_url, endpoint, single_id, sep = '/')

    execute_object <- DELETE(
      url = endpoint_url,
      content_type_json(),
      add_headers(`X-OB-Api` = api_key)
    )

    if (execute_object$status_code == 200) {
      svMisc::progress(value = i, max.value = nrow(data_to_delete))
    }
    else {
      print(paste0('Status Code is ', execute_object$status_code))
      break
    }
  }
}

