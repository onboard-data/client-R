# Import Libraries --------------------------------------------------------

#' @import dplyr
#' @import httr
#' @importFrom lubridate as_datetime
#' @importFrom tidyr replace_na
#' @importFrom data.table rbindlist
#' @importFrom rstudioapi askForSecret
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON

# Set API keys and URL ----------------------------------------------------------

#' @export
api.setup <- function(api_type) {

  if(missing(api_type)) {
    api_type <-'prod'
    api_url <- 'https://api.onboarddata.io'

    api_key <-rstudioapi::askForSecret(
      name='api_key_prod',
      message = 'Enter your API key here',
      title="Onboard API Keys")

  } else if (api_type == 'dev') {
    api_url <- 'https://devapi.onboarddata.io'

    api_key <- rstudioapi::askForSecret(
      name = 'api_key_dev',
      message='Enter your DEV API key here',
      title = "Onboard API Keys")
  }

assign('api_url', api_url, envir = parent.frame())

assign('api_key', api_key, envir = parent.frame())

#key_list()
# api_key_r <- key_get('RStudio Keyring Secrets',username = 'api_key')


}

#' @export
api.status <- function() {

  request <- GET(url = api_url,
                 add_headers(`X-OB-Api` = api_key))

  print(paste0("API Status: ", request$status_code))


}


# Basic API Query with endpoint -------------------------------------------

## Get API
#'@export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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


# Data Model --------------------------------------------------------------

##Query All Equipment Types
#' @export
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


# Point Selector ----------------------------------------------------------

#' @export
PointSelector <- function(){
  query <- list(orgs='',buildings='',point_ids='',point_names='',
                point_topics='',updated_since='',point_types='',
                equipment='',equipment_types='',point_hashes='')

  return(query)
}

#' @export
select_points <- function(query){

  if(query$updated_since!='') {
    updated_since <- as.numeric(as.POSIXct(query$updated_since))
  } else {
    updated_since<- ''
  }

  query <- query[names(query)!='updated_since']

  query_json <- query %>%
    toJSON()

  query_json <- gsub('\\[""\\]','[]',query_json)

  if(updated_since!=''){
    query_json <-
      gsub('}',paste0(',"updated_since":',updated_since,'}'),query_json)
  }

  endpoint_url <- paste(api_url,'points/select',sep='/')


  request_endpoint <- POST(url=endpoint_url,
                           content_type_json(),
                           add_headers(`X-OB-Api` = api_key),
                           body=query_json)


  if(request_endpoint$status_code==200){

    point_selector_output <- content(request_endpoint)

    return(point_selector_output)
  } else {
    stop(sprintf('API Status Code: %s',request_endpoint$status_code))
  }

}

#query points by id
#' @export
get_points_by_ids <- function(id){

  id_unlist <- unlist(id)

  chunks <- split(id_unlist,
                  ceiling(seq_along(id_unlist)/500))

  points<-data.frame()

  for(i in 1:length(chunks)){

    point_ids <- toJSON(chunks[[i]])

    #URL encode JSON list of points
    point_ids <- URLencode(point_ids,reserved = T)

    endpoint <- paste0('points?point_ids=',point_ids)

    points_chunk <- api.get(endpoint)

    points <- plyr::rbind.fill(points,points_chunk)
  }

  return(points)

}

#query equipment by id
#' @export
get_equipment_by_ids <- function(id){

  id_unlist <- unlist(id)

  #Convert list of ids to JSON payload
  id_json <- list(equipment_ids=id_unlist) %>%
    toJSON()

  equipment <- api.post(endpoint='equipment/query',
                        json_body = id_json)

  return(equipment)
}


# Building Data -----------------------------------------------------------

#Takes building id or name. Gives a text output of the building id and name
get_building_info <- function(buildings){

  query <- PointSelector()

  query$buildings <- buildings

  selection <- select_points(query)

  buildings_id <- unlist(selection$buildings)

  if(is.null(buildings_id)){
    stop('No building found.')
  }

  building_db <- api.get('buildings')

  building_db_filter <- building_db[building_db$id %in% buildings_id,]

  id <- building_db_filter$id
  name <- building_db_filter$name

  assign('id',id,envir=parent.frame())
  assign('name',name,envir = parent.frame())

  print(sprintf('Building ID: %s, Building Name: %s',id,name))

}

##Get clean metadata by building names or ids
#' @export
get_metadata <- function(buildings,selection){

  if(missing(selection) & missing(buildings)){
    stop('Provide either building name/id or selection list.')
  } else if (missing(selection)){

  get_building_info(buildings)

  query <- PointSelector()

  query$buildings <- buildings

  selection <- select_points(query)
  }

  print('Querying Points...')
  points <- get_points_by_ids(selection$points)

  print('Querying Equipment...')
  equipment <- get_equipment_by_ids(selection$equipment)

  #Create a metadata for the specified building ID
  metadata <- inner_join(equipment,points,
                         by = c('id' = 'equip_id'),
                         suffix = c('', '.y')) %>%
    #Get tagged units if NA
    mutate_at(vars(tagged_units),
              ~ifelse(is.na(.),units,as.character(.))) %>%
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
    mutate_all( ~ replace_na(as.character(.),'')) %>%
    #Convert unix time-stamps to EST
    mutate_at(vars(first_updated, last_updated),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York'))

  print('Metadata generated.')
  return(metadata)
}

# Staging Area API --------------------------------------------------------

##Get metadata from staging area
#' @export
get_staged_data <- function(building){

  get_building_info(building)

  print('Querying Staging Data...')

  endpoint <- paste0('staging/',id,'?points=True')

  stage <- api.get(endpoint)

  #Equip Data

  equip_data<- stage$equipment
  equip_data_names <- names(equip_data)
  equip_data_names <- gsub('data\\.','',equip_data_names)
  equip_data_names <- paste0('e.',equip_data_names)
  names(equip_data) <- equip_data_names

  equip_data_names <- data.frame(names=names(equip_data)) %>%
    filter(!grepl('confidences|\\.cnf|auto_tagger|\\.err|type_id|tags|created',
                  names))

  equip_data <- equip_data %>%
    select(equip_data_names$names)

  #Points Data

  points_list <- stage$points_by_equip_id

  points_data <- data.table::rbindlist(points_list,fill=T)

  points_data_names <- names(points_data)
  points_data_names <- gsub('data\\.','',points_data_names)
  points_data_names <- paste0('p.',points_data_names)
  names(points_data) <- points_data_names

  rem_col <- paste('auto_tagger','.cnf','polarity','reliability',
                   'activeText','event','covIncrement','presvalue',
                   'statusflags','outofservice','unit_id','type_id',
                   'datasource','limit','deadband','@prop','timedelay',
                   'notif','acked','resolution','state_text',
                   'relinquish','priority','p\\.e\\.','confidences',
                   'check','created',sep='|')

  points_data_names <- data.frame(names=points_data_names) %>%
    filter(!grepl(rem_col,names,ignore.case=T))

  points_data<- points_data %>%
    select(points_data_names$names)


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

#' @export
upload_staging <- function(building,
                           data_to_upload,
                           skip_topics){

  get_building_info(building)

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

    operation <- 'Skipping'
  } else {
    operation <- 'Uploading'
  }

  data_to_upload_json <- data_to_upload %>%
    toJSON()

  proceed <- askYesNo(sprintf('Do you want to proceed %s %s topics for %s?',operation,nrow(data_to_upload),name))

  if(proceed!=T){
    stop('Stopping Operation.')
  }

  print(sprintf('%s topics...',operation))

  #get endpoint
  endpoint <- paste0('staging/',id)

  post_points <- api.post(endpoint,json_body = data_to_upload_json)

  if(length(post_points$row_errors)==0){
    print('Success!')
  } else{
    return(post_points)
  }

}

##Promote valid data on the staging area to the live building

#' @export
promote_staged_data <- function(building,
                                data_to_promote){

  get_building_info(building)

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

      data_to_promote <- data_to_promote %>%
        filter(e.equip_id!='__SKIP__')

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

  endpoint <- paste0('staging/',id,'/apply')

  promote_data <- api.post(endpoint,
                           json_body = promote_json)


  validation_errors <-rrapply(promote_data,how='melt') %>%
    filter(L1!='building_id')

  if(nrow(validation_errors)!=0){

    validation_errors <- validation_errors %>%
      filter(L2!='__SKIP__',!is.na(L2))

    assign('validation_errors',
           validation_errors,
           parent.frame())

    stop('See validation_errors.')
  }
  else {
    print('Passed Validation.')
  }

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
      print(sprintf('Deleted %s of %s',i,
                    nrow(data_to_delete)))
    }
    else {
      print(paste0('Status Code is ', execute_object$status_code))
      break
    }
  }
}

