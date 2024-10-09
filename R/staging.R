# Staging Area --------------------------------------------------------

#' Create building
#' 
#' Creates an empty building on the staging area
#' 
#' @param building_name Enter the name of the building you want to create on the staging area
#' 
#' @param org_id  (Optional) If missing, the org_id will be grabbed from user's API access
#' 
#' @export
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

#' Get Staged Data
#' 
#' Gets metadata from the staging area.
#' 
#' @param building Character vector or integer corresponding to the building name or id. If you enter multiple building ids or names, only the first entry is considered.
#' 
#' @param verbose Logical. If TRUE (default), prints status and progress messages.
#' 
#' @return A data.frame of metadata from the staging area.
#' 
#' @export
get_staged_data <- function(building, verbose = TRUE){
  
  if(length(building)>1){
    stop('Length of building parameter greater than 1. Enter only one building id or name')
  }

  building_info <- get_building_info(building, verbose = verbose)

  if(verbose){
    cat('Querying staging data...\n')
  }

  endpoint <- paste0('staging/',building_info$id,'?points=True')

  stage <- api.get(endpoint)
  
  if(length(stage$points_by_equip_id) == 0){
    stop(sprintf('Staged data not found for building %s.', building_info$name))
  }
  
  #List of strings that filters non required columns from equip & point tables 
  rem_col <- paste('polarity','reliability',
                   'activeText','event','covIncrement','presvalue',
                   'statusflags','outofservice',
                   'datasource','limit','deadband','@prop',
                   'timedelay','instance_tagger','ob_predicted',
                   'notif','acked','resolution','state_text',
                   'relinquish','priority','p\\.data\\.e\\.','_tagger',
                   'p\\.confidences\\.e\\.',
                   'check','created','\\.err','tags',"type_id","unit_id",
                   #Device fields
                   "maxMaster","offset","objectList","systemStatus",
                   "maxInfo","protocolVersion","Revision","vendorId","lastRestart",
                   "Segment","apdu","binding","daylight","d.objectInstance",
                   "covsubscription","align","backup","restore","Sync",
                   sep='|')

  #Equip Data
  if(verbose){
    cat("Extracting equipment details...\n")
  }
  
  equip_data <- stage$equipment
  equip_data_names <- names(equip_data)
  equip_data_names <- paste0('e.', equip_data_names)
  names(equip_data) <- equip_data_names

  equip_data_names <- data.frame(names=names(equip_data)) %>%
    filter(!grepl(rem_col,names))

  equip_data <- equip_data %>%
    select(equip_data_names$names)
  
  #Device Data
  if(length(stage$devices_by_device_id)==0){
    
    #Temporary fix until all staging data is transformed
    device_data <- data.frame(d.device_id="NA")
  } else {
    if(verbose){
      cat("Extracting device details...\n")
    }
    
  device_list <- stage$devices_by_device_id
  
  device_list <- rlist::list.flatten(device_list)
  device_data <- Filter(function(x)length(x)==1, device_list)  %>% 
    bind_rows() %>%
    t() %>% 
    data.frame() %>% 
    rownames_to_column(var = "device_details") %>% 
    tidyr::separate(col="device_details",into=c("device","details"),
                    sep="\\.",extra = "merge") %>% 
    pivot_wider(id_cols = "device",
                names_from = "details",values_from = ".") %>% 
    select(-device)
  
  
  device_data_names <- names(device_data)
  device_data_names <- paste0('d.',device_data_names)
  names(device_data) <- device_data_names
  
  device_data_names <- data.frame(names=device_data_names) %>%
    filter(!grepl(rem_col,names,ignore.case=T))
  
  device_data<- device_data %>%
    select(device_data_names$names)
}
  #Points Data
  
 if(verbose){
    cat("Extracting point details...\n")
  }

  points_list <- stage$points_by_equip_id
  
  processed_points_list <- lapply(points_list,process_columns)
  
  points_data <- bind_rows(processed_points_list) %>% 
    tidyr::separate_rows(equip_ids,sep=", ") %>% 
      distinct(equip_ids,topic,.keep_all = TRUE)

  points_data_names <- names(points_data)
  points_data_names <- paste0('p.',points_data_names)
  names(points_data) <- points_data_names

  points_data_names <- data.frame(names=points_data_names) %>%
    filter(!grepl(rem_col,names,ignore.case=T))

  points_data<- points_data %>%
    select(points_data_names$names) 
  
staged_data <- full_join(points_data,equip_data,
                           by = c('p.equip_ids' = 'e.equip_id')) %>%
  rename(e.equip_id = p.equip_ids) %>% 
  full_join(device_data,
            by=c('p.device_id' = "d.device_id")) %>% 
  select(sort(tidyselect::peek_vars()))  %>% 
    #Convert epoch timestamps to UTC
    mutate(across(c(.data$e.last_promoted, .data$p.last_promoted,
                    .data$e.modified,.data$e.modified ,.data$p.modified, .data$p.last_updated),
                  ~ as.POSIXct(as.integer(substr(.,1,10)),
                               origin = '1970-01-01',
                               tz = 'UTC')))
  
  if(verbose){
    cat('Staging data created.\n')
  }

  return(staged_data)

}

##Upload data to the staging area or assign __SKIP__ equip_id to topics on the staging area
##skip_topics is optional (T or F)(Use with Caution)

#' Upload to Staging Area
#' 
#' Uploads data to the staging area.
#' 
#' @inheritParams get_staged_data
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

# Promote & Unpromote -----------------------------------------------------

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
#' @inheritParams get_staged_data
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

#' Unpromote Data from the live Building
#' 
#' @inheritParams get_staged_data
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
