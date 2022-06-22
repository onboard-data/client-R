# Import Libraries ---------------------------------------------------------------
library(tidyverse)
library(httr)
library(jsonlite)
library(keyring)
library(lubridate)
library(anytime)
library(rrapply)
options(stringsAsFactors = F)


# Set API keys and url ----------------------------------------------------------

api.setup <- function(api_type) {
  if(missing(api_type)) {
    api_type <-'prod'
    api_url <- 'https://api.onboarddata.io'
    
    rstudioapi::askForSecret(
      name='api_key_prod',
      message = 'Enter your API key here',
      title="Onboard API Keys")
    
    api_key <- key_get('RStudio Keyring Secrets',
                       username='api_key_prod')
    
  } else if (api_type == 'dev') {
    api_url <- 'https://devapi.onboarddata.io'
    
    rstudioapi::askForSecret(
      name = 'api_key_dev',
      message='Enter your DEV API key here',
      title = "Onboard API Keys")
    
    api_key <- key_get('RStudio Keyring Secrets',
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
      content(request_endpoint, as = 'text', encoding = 'UTF-8')  %>% 
      fromJSON(flatten = T) 
    
    return(api_output)
    
  } else{
    
    stop(paste0('API Status Code: ', request_endpoint$status_code))
    
  }
}


# Retrieve Database -------------------------------------------------------

##Orgs
get_orgs <- function(id){
  orgs <- api.get('organizations')
  
  orgs <- orgs$data
  
  if(!missing(id)){
    id = as.data.frame(id)
    
    orgs <- semi_join(orgs,id,
                               by='id')
  }
  
  return(orgs)
}

## Get User info
get_users <- function(){
  
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
  
  return(users)
}

## Get Deployment Stats
get_deployments <- function(){
  
  deployments <- api.get('deployment')
  
  deployments<- deployments %>%
    mutate_at(vars(last_heartbeat),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York')) %>%
    select(-api_key,-wg_pubkey) 
  
  return(deployments)
  
}


# Query Data Model --------------------------------------------------------

##Query All Equipment Types
get_equip_types <- function(){
  
equiptype <-api.get('equiptype')

#Get subtypes
subtype<-rrapply(equiptype$sub_types,how='melt') %>% 
  mutate_at(vars(value),
            ~gsub('c\\(|\\(|\\"|\\)','',.))  

char_split<-data.frame(str_split(subtype$value,', ',n=Inf,simplify=T))

subtype_format <- bind_cols(subtype,char_split) %>% 
  select(-value) %>% 
  filter(L2!='id')  %>% 
  pivot_longer(cols = c(-1,-2),names_to = 'names',values_to = 'values') %>% 
  filter(values!='') %>% 
  pivot_wider(id=c(L1,names),names_from = L2,values_from = 'values') %>% 
  select(-c(L1,names)) %>% 
  mutate_at(vars(equipment_type_id),~as.integer(.))

equip_types <- equiptype %>% 
  filter(active==T) %>% 
  select(-c(sub_types,critical_point_types,flow_order,active)) %>% 
  left_join(subtype_format,by=c('id'='equipment_type_id'),
            suffix = c('','_subtype'))

return(equip_types)
}

## Get Point types, measurements and their units in a clean output
# Enter T for write_file if you want to save to directory
get_point_types <- function(){
  
  pointtypes <- api.get('pointtypes')
  
  units <- api.get('unit')
  
  measurements <- api.get('measurements')  
  
  # Get measurements and their associated units
  measurement_unit_df <- data.frame()
  
  for (i in 1:nrow(measurements)){
    row_num =i
    
    measurement_single <- measurements[row_num,]
    
    
    unit_list <- measurement_single$units 
    
    if(!nrow(unit_list[[1]])==0){
      
      
      unit  <-  data.frame(t(sapply(unit_list,c))) %>% 
        select(unit_name=name_long,unit=name_abbr,data_type) %>% 
        mutate_all(~gsub('c\\("|"|\\)','',.)) %>% 
        mutate(name=measurement_single$name) 
      
      measurement_unit_df_single <- left_join(measurement_single,unit,
                                              by=c('name')) %>% 
        select(id,measurement_name=name,
               units_convertible,
               qudt_type,
               unit_name,
               unit,
               data_type)
      
      measurement_unit_df <- rbind(measurement_unit_df,measurement_unit_df_single)
    }
  }
  
  #Unite data frames
  point_types <- left_join(select(pointtypes,id,tag_name,measurement_id,tags),
                                      measurement_unit_df,
                                      by = c("measurement_id" = 'id')) %>% 
    mutate_at(vars(tags),  ~ gsub('c\\(|\\)', '', .)) %>% 
    select(id,
           point_type = tag_name,
           measurement_name,
           unit,
           data_type,
           tags)
  
  return(point_types)

}

# Get Building data ---------------------------------------------------------

get_building_info <- function(id,name){
  
  #Query all buildings and filter by name
  buildings <- api.get('buildings')
  
  if(missing(id) &
     missing(name)) {
    stop('Please provide the building id or building name')
    
  } else if (missing(id)) {
    
    id <- buildings$id[which(buildings$name == name)]
    
    if (is_empty(id)) {
      stop('Please enter the correct building name. Refer to db_buidings.')
      
    } else{
      
      
    }} else if (missing(name)) {
      
      if (id %in% buildings$id) {
        name <- buildings$name[which(buildings$id == id)]
        
      } else {
        stop('Building id not found. Refer to db_buidings.')
        
      }} else {
        building_id <- buildings$id[which(buildings$name==name)]
        
        if(id!=building_id){
          print('Building id does not match with building name')
          stop(sprintf('Building id for %s is %s. Refer to db_buidings.',name,building_id))
          
        }
      }
  
  assign('id',id,envir=parent.frame())
  assign('name',name,envir = parent.frame())

  print(sprintf('Building ID: %s, Building Name: %s',id,name))
  
}

##Get metadata from live building
get_metadata <- function(id,name){
  
  get_building_info(id,name)
  
  # Grab Equipment Data for the specified Building ID
  equipment <- api.get(paste0('buildings/', id, '/equipment'))
  
  # Grab Points Data for the specified Building ID
  points <- api.get(paste0('buildings/', id, '/points'))
  
  #Create a metadata for the specified building ID
  metadata <- inner_join(equipment,points,
                             by = c('id' = 'equip_id'),
                             suffix = c('', '.y')) %>% 
    select(
      building_id,
      equipment_id = id,
      point_id = id.y,
      device,
      objectId,
      name,
      description,
      first_updated,
      last_updated,
      value,
      tagged_units,
      point_type=type,
      equip_type=equip_type_tag,
      suffix,
      equip_id,
      parent_equip,
      floor_num_physical,
      floor_num_served,
      area_served_desc,
      topic
    ) %>%
    # Grab Equip Refs by joining with Equip DB again
    mutate(parent_equip = as.integer(parent_equip)) %>%
    left_join(
      select(equipment, id, equip_id),
      by = c('parent_equip' = 'id'),
      suffix = c('', '.y')
    ) %>%
    select(everything(),
           equip_ref = equip_id.y,
           -parent_equip) %>%
    # remove NAs
    mutate_all( ~ replace_na(., '')) %>% 
    #Convert unix time-stamps to EST
    mutate_at(vars(first_updated, last_updated),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York')) 
 return(metadata)
}


# Staging Area API  ---------------------------------------------------------

##Get metadata from staging area
get_staged_data <- function(id,name){
  
  get_building_info(id,name)
  
  #get endpoint
  endpoint_url <- paste0(api_url,'/staging/',id,'?points=True')
  
  print(sprintf('Fetching Data for %s...',name))
  
  request_endpoint <- GET(url = endpoint_url,
                          content_type_json(),
                          add_headers(`X-OB-Api` = api_key))
  
  request_content <- content(request_endpoint)  
  
  equip_list <- request_content$equipment
  
  if(length(equip_list)==0){
    print('No data found.')
    break()
  }
  
  equip_data <- rrapply(equip_list,how='melt') %>% 
    mutate_at(vars(L3),
              ~ifelse(is.na(L3),L2,.)) %>% 
    filter(!grepl('auto_tagger|type_id|.cnf|.err',L3),
           !grepl('confidences',L2))  %>% 
    distinct() %>% 
    pivot_wider(id_cols=L1,names_from='L3',values_from='value') %>% 
    mutate_all(~as.character(.)) %>% 
    mutate_all(~ifelse(.=='NULL','',.)) %>% 
    select(-c(L1,created))
  
  colnames(equip_data) <- paste0('e.',colnames(equip_data))
  
  points_list <- request_content$points_by_equip_id
  
  rem_col <- paste('auto_tagger','.cnf','polarity','reliability','activeText','event',
                   'covIncrement','presvalue','statusflags','outofservice','unit_id','type_id',
                   'datasource','limit','deadband','@prop','timedelay','notif','acked',
                   'resolution','state_text','relinquish','priority',
                   sep='|')
  
  points_data <- rrapply(points_list,how='melt') %>% 
    mutate_at(vars(L4),
              ~ifelse(is.na(L4),L3,.)) %>% 
    filter(!grepl(rem_col,L4,ignore.case = T),
           !grepl('confidences',L3)) %>% 
    distinct() %>% 
    pivot_wider(id_cols=c(L1,L2),names_from='L4',values_from='value') %>% 
    mutate_all(~as.character(.)) %>% 
    mutate_all(~ifelse(.=='NULL','',.)) %>% 
    select(-c(L1,L2,created,starts_with('e.')))
  
  
  colnames(points_data) <- paste0('p.',colnames(points_data))
  
  
  staged_data <- left_join(equip_data,
                            points_data,
                            by=c('e.equip_id'='p.equip_id')) %>% 
    mutate_at(vars(e.last_promoted,p.last_promoted,e.modified,p.modified),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York')) %>%
    select(sort(tidyselect::peek_vars()))
  
  return(staged_data)
  
} 

##Upload data to the staging area or assign __SKIP__ equip_id to topics on the staging area
##skip_topics is optional (T or F)(Use with Caution)

upload_staging <- function(id,name,
                               data_to_upload,
                               skip_topics){
  
  get_building_info(id,name)
  
  if (missing(skip_topics)){
    skip_topics <- F
  }
  
  if(missing(data_to_upload)) {
    
    stop('data_to_upload is missing in the function call. data_to_upload should be a dataframe including atleast e.equip_id & p.topic for the upload to succeed')
    
  } else if (!('p.topic' %in% names(data_to_upload))) {
    print('p.topic column not found in staging_upload.')
    
  } else if (!('e.equip_id' %in% names(data_to_upload)) &
             skip_topics == F) {
    stop('e.equip_id column not found in data_to_upload.')
  } else if (skip_topics ==T){
    data_to_upload <- data_to_upload %>% 
      transmute(e.equip_id = '__SKIP__', p.topic)
    
    operation <- 'skipping'
  } else {
    operation <- 'uploading'
  }
  
  data_to_upload_json <- data_to_upload %>% 
    toJSON()
  
  proceed <- askYesNo(sprintf('Do you want to proceed %s %s topics for %s?',operation,nrow(data_to_upload),name))
  
  if(proceed!=T){
    stop('Stopping Operation.')
  }
  
  print(sprintf('%s topics...',operation))
  
  #get endpoint
  endpoint_url <- paste0(api_url,'/staging/',id)
  
  post_endpoint <- POST(url = endpoint_url,
                        content_type_json(),
                        add_headers(`X-OB-Api` = api_key),
                        body=data_to_upload_json)
  
  if (post_endpoint$status_code == 200) {
    
    print(sprintf(
      'Success %s %s topics for building id: %s',
      operation,nrow(data_to_upload),id))
  }else {
    
    stop(print(sprintf('Status Code is %s',post_endpoint$status_code)))
    
  }
}

##Promote valid data on the staging area to the live building 

promote_staged_data <- function(id,name,
                                    data_to_promote){
  
  get_building_info(id,name)
  
  if(missing(data_to_promote)){
    
    proceed <- askYesNo(sprintf('Do you want to proceed with promoting all valid topics for %s?',name))
    
    if(proceed==T){
      
      promote_json <- list(equip_ids='',topics='') %>% toJSON()
      
      promote_json <- gsub('\\["','[',promote_json)
      promote_json <- gsub('"\\]',']',promote_json)
      
      operation <- 'promote_all'
      
    } else {
      stop('Stopping Operation.')
      
    }} else if (!('p.topic' %in% names(data_to_promote))) {
      print('p.topic column not found in data_to_promote.')
      
    } else if (!('e.equip_id' %in% names(data_to_promote))) {
      stop('e.equip_id column not found in data_to_promote')
    } else {
      
      equip_count <-length(unique(data_to_promote$e.equip_id))
      
      proceed <-
        askYesNo(
          sprintf(
            'Do you want to proceed with promoting %s equipment and their valid topics to %s?',equip_count,name))
      
      if(proceed==T){
        
        promote_json <- list(equip_ids=data_to_promote$e.equip_id,topics=data_to_promote$p.topic) %>%  
          toJSON()
        
        operation <- 'promote_some'
      } else{
        stop('Stopping Operation.')
      }
    }
  print('Validating data...')
  api.get(paste('staging',id,'validate',sep='/'),supress_message = T)
  
  if(length(validate$points)!=0&
     length(validate$equipment)!=0){
    
    validation_errors <- do.call(bind_cols,validate) %>% 
      pivot_longer(cols = -building_id,names_to = 'p.topic',
                   values_to='Validaton Error') %>% 
      distinct()
    
    if(operation=='promote_some'){
      validation_errors <- semi_join(validation_errors,
                                     select(data_to_promote,p.topic),
                                     by='p.topic') 
    }
    
    if(nrow(validation_errors)!=0){
      assign('validation_errors',validation_errors,parent.frame())
      stop('See validation_errors.')
    }
    else {
      print('Passed Validation...')
    }
    
  }else {
    print('Passed Validation...')
  }
  
  endpoint_url <- paste0(api_url,'/staging/',id,'/apply')
  
  post_endpoint <- POST(url = endpoint_url,
                        content_type_json(),
                        add_headers(`X-OB-Api` = api_key),
                        body=promote_json)
  
  if (post_endpoint$status_code == 200) {
    
    if(operation=='promote_all'){
      print(sprintf('Success!! Promoted staged data for %s',name))
    } else if(operation=='promote_some'){
      print(sprintf('Success!! Promoted %s staged equipment/s for %s',equip_count,name))
    } else {
      stop(sprintf('Status Code is %s',post_endpoint$status_code))
    }
    
    
  }
}

# Delete equipment or points from live buildings --------------------------

##Delete points/equipment from live data. Use with caution
delete_data <- function(endpoint,data_to_delete){
  
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

