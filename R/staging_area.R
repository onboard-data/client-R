# Staging Area --------------------------------------------------------

#' Get Staged Data
#' 
#' Gets metadata from the staging area.
#' 
#' @param building Enter building id or name. Note: If you enter multiple building ids or names, only the first entry is considered.
#' 
#' @export
get_staged_data <- function(building){
  
  if(length(building)>1){
    stop('Length of building parameter greater than 1. Enter only one building id or name')
  }

  building_info <- get_building_info(building)

  print('Querying staging data...')

  endpoint <- paste0('staging/',building_info$id,'?points=True')

  stage <- api.get(endpoint)
  
  if(length(stage$points_by_equip_id) == 0){
    stop(sprintf('Staged data not found for building %s.', building_info$name))
  }

  #Equip Data
  
  equip_data <- stage$equipment
  equip_data_names <- names(equip_data)
  equip_data_names <- gsub('data\\.','', equip_data_names)
  equip_data_names <- paste0('e.', equip_data_names)
  names(equip_data) <- equip_data_names

  equip_data_names <- data.frame(names=names(equip_data)) %>%
    filter(!grepl('confidences|\\.cnf|auto_tagger|\\.err|type_id|tags|created|guid',
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
                   'datasource','limit','deadband','@prop',
                   'timedelay',
                   'notif','acked','resolution','state_text',
                   'relinquish','priority','p\\.e\\.','confidences',
                   'check','created',sep='|')

  points_data_names <- data.frame(names=points_data_names) %>%
    filter(!grepl(rem_col,names,ignore.case=T))

  points_data<- points_data %>%
    select(points_data_names$names)


  staged_data <- left_join(equip_data,
                           points_data,
                           by = c('e.equip_id' = 'p.equip_id')) %>%
    mutate(across(c(.data$e.last_promoted, .data$p.last_promoted,
                    .data$e.modified, .data$p.modified),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York'))) %>%
    select(sort(tidyselect::peek_vars()))
  
  print('Staging data created.')

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
#' @param skip_topics Logical. False (default). If True, the uploaded topics will be assigned `__SKIP__` equip_id.
#'  
#'@export
upload_staging <- function(building,
                           data_to_upload,
                           skip_topics = F){

  building_info <- get_building_info(building)

  if(missing(data_to_upload)) {

    stop('data_to_upload is missing in the function call. data_to_upload should be a dataframe including at least e.equip_id & p.topic for the upload to succeed')

  } else if (!('p.topic' %in% names(data_to_upload))) {
    print('p.topic column not found in staging_upload.')

  } else if (!('e.equip_id' %in% names(data_to_upload)) &
             skip_topics == F) {
    stop('e.equip_id column not found in data_to_upload.')
  } else if (skip_topics ==T){
    data_to_upload <- data_to_upload %>%
      transmute(e.equip_id = '__SKIP__', .data$p.topic)

    operation <- 'skipping'
  } else {
    operation <- 'uploading'
  }

  data_to_upload_json <- data_to_upload %>%
    toJSON()

  proceed <- askYesNo(sprintf('Do you want to proceed %s %s topic/s for %s?', 
                              operation,nrow(data_to_upload), building_info$name))

  if(is.na(proceed)|proceed!=T){
    stop('Stopping Operation.')
  }

  print(sprintf('%s topics...', operation))

  #get endpoint
  endpoint <- paste0('staging/', building_info$id)

  post_points <- api.post(endpoint,
                          json_body = data_to_upload_json)

  if(length(post_points$row_errors)==0){
    print('Success!')
  } else{
    return(post_points$row_errors)
  }

}


#' Promote data on Staging Area
#' 
#' Promote valid data on the staging area to the live building.
#' 
#' @inheritParams get_staged_data
#' 
#' @param data_to_promote (Optional) If missing, all valid topics are promoted. A dataframe containing e.equip_id & p.topic columns
#' 
#' @export
promote_staged_data <- function(building, data_to_promote){

  building_info <- get_building_info(building)
  
  if(missing(data_to_promote)){

    proceed <- askYesNo(
      sprintf('Do you want to proceed with promoting all valid topics for %s?', building_info$name))

    if(is.na(proceed)|proceed!=T){
      stop('Stopping Operation.')
    }

      promote_json <- list(equip_ids='',
                           topics='') %>%
        toJSON()

      promote_json <- gsub('\\["','[',promote_json)
      promote_json <- gsub('"\\]',']',promote_json)

      operation <- 'promote_all'

    } else {

      data_to_promote <- data_to_promote %>%
        filter(.data$e.equip_id != '__SKIP__')

      equip_count <-length(unique(data_to_promote$e.equip_id))

      proceed <-
        askYesNo(
          sprintf('Do you want to proceed with promoting %s equipment and their valid topics to %s?', equip_count, building_info$name))

      if(is.na(proceed)|proceed != T){
        stop('Stopping Operation.')
      }

        promote_json <- list(equip_ids = data_to_promote$e.equip_id,
                             topics = data_to_promote$p.topic) %>%
          toJSON()

        operation <- 'promote_some'
    }

  endpoint <- paste0('staging/', building_info$id, '/apply')

  promote_data <- api.post(endpoint,
                           json_body = promote_json)

  #Get Validation Errors

  point_errors <- as.data.frame(do.call(rbind,
                          promote_data$points)) %>%
    tibble::rownames_to_column(var = 'p.topic')

  equipment_errors <- as.data.frame(do.call(
    rbind, promote_data$equipment)) %>%
    tibble::rownames_to_column(var = 'e.equip_id')

  validation_errors <- plyr::rbind.fill(point_errors,
                                equipment_errors)  %>% 
  mutate(across(everything(),
                ~tidyr::replace_na(as.character(.),''))) %>%
   filter(.data$e.equip_id != '__SKIP__')

  if('V1' %in% names(validation_errors)){

    validation_errors <- rename(validation_errors,
                                'errors' = 'V1') %>%
      filter(.data$errors != 'NULL')

    assign('validation_errors',
           validation_errors,
           parent.frame())

    print('See validation_errors.')
  } else {
    print('Promoted.')
  }

}