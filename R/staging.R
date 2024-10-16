# Create Building (Deprecated) --------------------------------------------------------

#' Create building (Deprecated)
#' 
#' Creates an empty building on the staging area
#' 
#' @param building_name Enter the name of the building you want to create on the staging area
#' 
#' @param org_id  (Optional) If missing, the org_id will be grabbed from user's API access
#' 
create_building <- function(building_name,org_id = NULL, verbose = TRUE){
  
  #If org_id is not provided, grab an org_id from user's API access
  if(is.null(org_id)){
    org_id <- api.get("whoami")$userInfo$org_id
  }
  
  orgs <- api.get("organizations")$data
  org_name <- orgs$name[which(orgs$id == org_id)]
  
  building_json <- data.frame(name = building_name,  
                              org_id = org_id) %>% 
    toJSON()
  
  building_json <- gsub("\\[|\\]","",building_json)
  
  create_building <- api.post(endpoint = "buildings",
           json_body =  building_json
          )

  if(verbose){
  print(sprintf("Success!! Building %s created for %s\n",building_name,org_name))
  }
} 

# Equipment ----------------------------------------------------

#' Get Staging Equip
#' 
#' Get unfiltered staging details on the building equipment
#' 
#' @param building_id Integer corresponding to the building id
#' 
#' @return A data.frame of staging equipment
#' 
#' @export
get_staging_equip <- function(building_id, verbose = TRUE){    
  
  if(verbose){
    cat("Getting staging equipment...\n")
  }
  
  endpoint <- paste0('staging/',building_id)
  
  staging_equip <- api.get(endpoint)$equipment
  
  return(staging_equip)
}


# Devices ------------------------------------------------------

#' Get Staging Devices
#' 
#' Get unfiltered staging details on the building devices
#' 
#' @inheritParams get_staging_equip
#' 
#' @return A data.frame of staging devices
#' 
#' @export
get_staging_devices <- function(building_id, verbose = TRUE) {
  
  if(verbose){
    cat("Getting staging devices...\n")
  }
  
  endpoint <- paste0('staging/',building_id,'/devices')
  
  staging_devices <- api.get(endpoint)
  
  return(staging_devices)
}  

# Points -------------------------------------------------------

#' Get Staging Points
#' 
#' Get unfiltered staging details on the building points
#' 
#' @inheritParams get_staging_equip
#' 
#' @return A data.frame of staging points
#' 
#' @export
get_staging_points <- function(building_id, verbose = TRUE){
  
  if(verbose){
    cat("Getting staging points...\n")
  }
  
  endpoint <- paste0('staging/',building_id,'/points')
  
  staging_points <- api.get(endpoint)
 
  return(staging_points) 
}


# Combined Data --------------------------------------------------------

#' Get Staging Data
#' 
#' Gets metadata from the staging area.
#' 
#' @param building Character vector or integer corresponding to the building name or id. If you enter multiple building ids or names, only the first entry will be considered.
#' 
#' @param verbose Logical. If TRUE (default), prints status and progress messages.
#' 
#' @return A data.frame of metadata from the staging area.
#' 
#' @export
get_staging_data <- function(building, verbose = TRUE){
  
  if(length(building)>1){
    stop('Length of building parameter greater than 1. Enter only one building id or name')
  }

  building_info <- get_building_info(building, verbose = verbose)
  
  building_id = building_info$id
  
  #List of strings that filters non required columns from equip, point & device tables 
  rem_col <- paste('polarity','reliability',
                   'event','covIncrement','presvalue',
                   'statusflags','outofservice',
                   'datasource','limit','deadband','@prop',
                   'timedelay','instance_tagger','ob_predicted',
                   'notif','acked','resolution','state_text',
                   'relinquish','priority','p\\.data\\.e\\.','_tagger',
                   'p\\.confidences\\.e\\.',
                   'check','\\.err','tags',"type_id","unit_id",
                   #Device fields
                   "maxMaster","offset","objectList","systemStatus",
                   "maxInfo","protocolVersion","Revision","vendorId","lastRestart",
                   "Segment","apdu","binding","daylight","d.objectInstance",
                   "covsubscription","align","backup","restore","Sync",
                   sep='|')
  
#Get Staging Equip  
staging_equip <- get_staging_equip(building_id = building_id, verbose = verbose)
  
##Organize & Filter Staging Equip
  staging_equip_names <- names(staging_equip)
  staging_equip_names <- paste0('e.', staging_equip_names)
  names(staging_equip) <- staging_equip_names

  staging_equip_names <- data.frame(names=names(staging_equip)) %>%
    filter(!grepl(rem_col,names))

  staging_equip <- staging_equip %>%
    select(staging_equip_names$names)

#Get Staging Devices  
staging_devices <- get_staging_devices(building_id = building_id, verbose = verbose)
  
  if(nrow(staging_devices)==0){
    
    #Temporary fix until all staging data is transformed
    staging_devices <- data.frame(d.device_id="NA")
    
    if(verbose){
      cat("No device details found...\n")
    }
    
  } else {

  #Organize & Filter Staging Devices 
  staging_devices_names <- names(staging_devices)
  staging_devices_names <- paste0('d.',staging_devices_names)
  names(staging_devices) <- staging_devices_names
  
  staging_devices_names <- data.frame(names=staging_devices_names) %>%
    filter(!grepl(rem_col,names,ignore.case=T))
  
  staging_devices<- staging_devices %>%
    select(staging_devices_names$names)
  }
  
# Get Staging Points

staging_points <- get_staging_points(building_id = building_id, verbose = verbose)

  if(nrow(staging_points) == 0){
    stop(sprintf('No points found for building %s.', building_info$name))
  }  

##Organize & Filter Staging Points  
  staging_points_names <- names(staging_points)
  staging_points_names <- paste0('p.',staging_points_names)
  names(staging_points) <- staging_points_names

  staging_points_names <- data.frame(names=staging_points_names) %>%
    filter(!grepl(rem_col,names,ignore.case=T))

  staging_points <- staging_points %>%
    select(staging_points_names$names) %>% 
    mutate(across(p.equip_ids,~as.character(.)))
  
staging_data <- full_join(staging_points,staging_equip,
                           by = c('p.equip_ids' = 'e.equip_id')) %>%
  full_join(staging_devices,
            by=c('p.device_id' = "d.device_id")) %>% 
  select(sort(tidyselect::peek_vars()))   
    # #Convert epoch timestamps to UTC
    # mutate(across(c(.data$e.last_promoted, .data$p.last_promoted,
    #                 .data$e.modified, .data$e.created, .data$p.created ,
    #                 .data$p.modified, .data$p.last_updated),
    #               ~ as.POSIXct(as.integer(substr(.,1,10)),
    #                            origin = '1970-01-01',
    #                            tz = 'UTC')))
  
  if(verbose){
    cat('Staging data created.\n')
  }

  return(staging_data)

}


# Upload -------------------------------------------------------


##Upload data to the staging area or assign __SKIP__ equip_id to topics on the staging area
##skip_topics is optional (T or F)(Use with Caution)

#' Upload to Staging Area
#' 
#' Uploads data to the staging area.
#' 
#' @inheritParams get_staging_data
#' 
#' @param data_to_upload A data.frame to upload to the staging area. Must contain e.equip_id and p.topic columns.
#' 
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting  
#'
#' @return A named list containing any errors that may have occured during data upload.
#'  
#'@export
upload_staging <- function(building,
                           data_to_upload,
                           proceed= NULL,
                           verbose = TRUE
                           ){

  building_info <- get_building_info(building, verbose = verbose)

  if(missing(data_to_upload)) {

    stop('data_to_upload is missing in the function call. data_to_upload should be a dataframe including at least e.equip_id & p.topic for the upload to succeed')

  } else if (!any(c('p.topic','e.equip_id') %in% colnames(data_to_upload))) {
    stop('Please include the p.topic or e.equip_id column in data_to_upload')
  }

  data_to_upload_json <- data_to_upload %>%
    toJSON()
  
if(is.null(proceed)){
  proceed <- askYesNo(sprintf('Do you want to proceed uploading %s point/s for %s?', 
                              nrow(data_to_upload), building_info$name))
}

  if(is.na(proceed) | proceed != TRUE){
    stop('Stopping Operation.')
  }

  if(verbose){
    cat('Uploading...\n')
  }

  #get endpoint
  endpoint <- paste0('staging/', building_info$id)
  
  post_points <- api.post(endpoint,
                          json_body = data_to_upload_json)
  
  output <- post_points$row_errors

  if(length(output)==0){
    message <- "Upload successful \n"
  } else{
    message <- "Upload unsuccesful. Please check errors for more information. \n"
  }
  
  if(verbose){
    cat(message)
  }
  
  if(length(output) != 0){
  return(output)
}
}

# Promote -----------------------------------------------------

api.promote <- function(building_id, payload_json, verbose){  
  
  endpoint <- paste0('staging/', building_id, '/apply')
  
  promotion <- api.post(endpoint,
                        json_body = payload_json)
  
  errors<-rrapply::rrapply(promotion,
                           how="melt") %>% 
    filter(.data$L1!="building_id") %>%  
    filter(!grepl(
      "Skipped (equipment|points are) not validated|No valid equipment for topic",
      value
    ))  
  
  
  if(nrow(errors != 0)){
    
    if(verbose){
      cat("Invalid data found in staging area. Please check the errors.\n")
    }
    
    errors <- errors %>% 
      rename(type = .data$L1, topic = .data$L2, error = .data$value)
    
    points_error <- errors[errors$type =="points",] 
    
    if(nrow(points_error)!=0){
      points_error_list <- setNames(split(points_error$error, 
                                          seq(nrow(points_error))),
                                    points_error$topic)
    } else {
      points_error_list <- NULL
    }
    
    
    equip_error <- errors[errors$type =="equipment",] 
    
    
    if(nrow(equip_error)!=0){
      equip_error_list <- setNames(split(equip_error$error, 
                                         seq(nrow(equip_error))),
                                   equip_error$topic)
    } else {
      equip_error_list <- NULL
    }
    
    unexp_del <-errors[errors$type =="unexpected_deletes",
                       "error"]
    
    output <- list("points" =points_error_list,
                   "equipment" = equip_error_list,
                   "unexpected_deletes" = unexp_del)
    
    return(output)
    
  } else {
    if(verbose){
      print("Operation Successful!\n")
    }
  }
}

#' Promote data on Staging Area
#' 
#' Promote valid data on the staging area to the live building.
#' 
#' @inheritParams get_staging_data
#' 
#' @param data_to_promote (Optional) If missing, all valid topics are promoted. A data.frame containing columns 'e.equip_id' & 'p.topic'.
#' 
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
#' 
#' @return (Conditional) A named list containing errors that may have occurred during data promotion.
#' 
#' @export

promote_data <- function(building, 
                         data_to_promote,
                         proceed = NULL,
                         verbose = TRUE){

  building_info <- get_building_info(building, verbose = verbose)
  
  if(missing(data_to_promote)){

    proceed <- askYesNo(
      sprintf('Do you want to proceed with promoting all valid topics for %s?', building_info$name)
      )

    if(is.na(proceed) | proceed != TRUE){
      stop('Stopping Operation.')
    }

      promote_json <- list(equip_ids='',
                           topics='') %>% 
        toJSON()
      
      promote_json <- gsub('\\["', '[', promote_json)
      promote_json <- gsub('"\\]', ']', promote_json)

      operation <- 'promote_all'

    } else {

      data_to_promote <- data_to_promote %>%
        filter(.data$e.equip_id != '__SKIP__')

      equip_count <-length(unique(data_to_promote$e.equip_id))

      if(is.null(proceed)){
      proceed <-
        askYesNo(
          sprintf('Do you want to proceed with promoting %s equipment and their valid topics to %s?', equip_count, building_info$name))
      }

      if(is.na(proceed)|proceed != TRUE){
        stop('Stopping Operation.')
      }

        promote_list <- list(equip_ids = data_to_promote$e.equip_id,
                             topics = data_to_promote$p.topic) 
        
        promote_json <- promote_list %>%  toJSON()

        operation <- 'promote_some'
    }
  
  api.promote(building_id = building_info$id,
              payload_json = promote_json,
              verbose = verbose)
  
}


# Unpromote ---------------------------------------------------------------


#' Unpromote Data from the live Building
#' 
#' @inheritParams get_staging_data
#' 
#' @param points_to_unpromote A data.frame containing columns 'e.equipment_id' & 'p.point_id'.
#' 
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
#' 
#' @return (Conditional) A named list containing errors that may have occurred during data promotion.
#' 
#' @export
unpromote_data <- function(building,points_to_unpromote = NULL, equipment_to_unpromote = NULL, proceed = NULL, verbose= TRUE){
  
  if(missing(points_to_unpromote) & missing(equipment_to_unpromote)) {
  
    stop('Please provide either points_to_unpromote or equipment_to_unpromote. It should be a dataframe including e.equipment_id & p.point_id for the unpromote to succeed.\n ')
  } 
  
  if (missing(equipment_to_unpromote)){
    
    equipment_point_pairs <- points_to_unpromote %>% 
      distinct()
    
    equipment_ids = 0

  } else if(missing(points_to_unpromote)) {
    
    equipment_point_pairs <- equipment_to_unpromote %>% 
      distinct()
    
    equipment_ids = unique(equipment_point_pairs$e.equipment_id)
  }
  
  if('p.point_id' %in% names(equipment_point_pairs)){
    point_ids = equipment_point_pairs$p.point_id  
    
    equipment_point_pairs <- equipment_point_pairs %>% 
      rename(equipment_id = e.equipment_id, point_id = p.point_id)
    
  } else {
    equipment_point_pairs <- data.frame(
      equipment_id = 0, point_id = 0)
    
    point_ids = 0
  }

  building_info <- get_building_info(building)
  
  if(is.null(proceed)){
    proceed <-
      askYesNo(
        sprintf('Do you want to proceed with unpromoting %s points across %s equipment from %s?',
                nrow(equipment_point_pairs),
                length(unique(equipment_point_pairs$e.equipment_id)),
                building_info$name))
  }
  
  if(is.na(proceed) | proceed != TRUE){
    stop('Stopping Operation.\n')
  }
  
unpromote_list <- list(equipment_ids = equipment_ids,
                       point_ids = point_ids,
                       point_equipment_relationships = equipment_point_pairs)

unpromote_json <- unpromote_list %>% toJSON()

endpoint <- paste0('staging/', building_info$id, '/apply')

json_body <- unpromote_json

api.delete(endpoint,json_body)

}
