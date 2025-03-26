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
get_staging_equip <- function(building_id, verbose = TRUE) {
  if (verbose)
    cat("Fetching staged equipment...\n")
  
  endpoint <- sprintf("staging/%d", building_id)
  
  api.get(endpoint, verbose = verbose)$equipment
  
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
  if (verbose)
    cat("Fetching staged devices...\n")
  
  endpoint <- sprintf("staging/%d/devices", building_id)
  api.get(endpoint, verbose = verbose)
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
get_staging_points <- function(building_id, verbose = TRUE) {
  if (verbose)
    cat("Fetching staged points...\n")
  
  endpoint <- sprintf("staging/%d/points", building_id)
  api.get(endpoint, verbose = verbose)
}


# Combined Data --------------------------------------------------------

#' Get Staging Data
#'
#' Gets metadata from the staging area.
#'
#' @param buildings Enter building names or ids. You can enter multiple building ids or names as vectors 
#'
#' @param verbose Logical. If TRUE (default), prints status and progress messages.
#'
#' @return A data.frame of metadata from the staging area.
#'
#' @export
get_staging_data <- function(buildings, verbose = TRUE) {
  buildings <- search_buildings(buildings = buildings, verbose = verbose)
  
  #Fetch Staging Data for all buildings found
  staging_data <- data.frame()
  
  for (i in 1:nrow(buildings)) {
    building_id <- buildings$id[i]
    
    building_name <- buildings$name[i]
    
    if (verbose) {
      cat(sprintf("Fetching staging data for %s. \n", building_name))
    }
    
    # Fetch Staging Points
    staging_points_single <- get_staging_points(building_id, verbose = verbose)
    if (nrow(staging_points_single) == 0) {
      if (verbose) {
        cat(sprintf("No points found for %s. \n", building_name))
      }
    } else {
      staging_points_single <- prefix_column_names(staging_points_single, "p") %>%
        mutate(across(p.equip_ids, ~ gsub('c\\(|\\)|"', "", .))) %>% # Remove "c()" if present
        separate_rows(p.equip_ids, sep = ",\\s*") %>% # Split by comma and optional space
        mutate(across(p.equip_ids, ~ gsub("character\\(0", "", .))) %>%
        mutate(across(p.equip_ids, ~ as.character(.)))
    
    }
    
    #Fetch Staging Equip
    staging_equip_single <- get_staging_equip(building_id, verbose = verbose)
    if (nrow(staging_equip_single) == 0) {
      if (verbose) {
        cat(sprintf("No equipment found for %s. \n", building_name))
      }
    } else{
      staging_equip_single <- prefix_column_names(staging_equip_single, "e")
    }
    
    #Fetch Staging Devices
    staging_devices_single <- get_staging_devices(building_id, verbose = verbose)
    if (length(staging_devices_single) == 0) {
      if (verbose) {
        cat(sprintf("No devices found for %s. \n", building_name))
        staging_devices_single <- data.frame(d.device_id=NA)
      }
    } else {
      staging_devices_single <- prefix_column_names(staging_devices_single, "d") %>% 
        mutate(building_name = building_name)
    }
  
  
  # Combine data
  staging_data_single <- full_join(staging_points_single,
                            staging_equip_single,
                            by = c("p.equip_ids" = "e.equip_id")) %>%
    full_join(staging_devices_single, by = c("p.staging_device_id" = "d.staging_id")) %>%
    dplyr::rename(building_id = d.building_id) %>% 
    select(sort(tidyselect::peek_vars())) %>%
    distinct()
  
  staging_data <- plyr::rbind.fill(staging_data,staging_data_single)

  }  

  #Remove certain columns
  staging_data <- select(staging_data,-contains(c("_tagger","state_text","@prop")))
  
  if (verbose) {
    cat("Staging data created.\n")
  }
  
  return(staging_data)
}


# Update Staging -------------------------------------------------------

#' Update Staging Points
#'
#' Update points on the staging area.
#'
#' @param building Character vector or integer corresponding to the building name or id.
#'
#' @param staging_update A data.frame to upload to the staging area. Must contain equip_names and topic columns. point_type_tag_name and raw_unit are optional columns
#'
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
#'
#' @return Result output of the update
#'
#'@export
update_staging_points <- function(building, 
                                  staging_update, 
                                  proceed = NULL, verbose = TRUE){
  
  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")
  
  # Get building info
  building_info <- search_buildings(buildings = building, verbose = verbose)
  
  required_cols <- c("equip_names", "topic")
  
  staging_update_cols <- names(staging_update)
  
  if(!all(required_cols %in% staging_update_cols)){
    stop(sprintf(
      "staging_update is missing cols %s",
      paste(required_cols, collapse = " or ")
    ))
  }
  
  if('raw_unit' %in% staging_update_cols){
    #Getting units to match with ids
    units <- api.get("unit",verbose = FALSE) %>% 
      select(raw_unit = name_abbr,raw_unit_id = id)
    
    staging_update <- left_join(staging_update,units, by =c("raw_unit"))
  }
  
  #Select columns
  optional_cols <- c("point_type_tag_name","raw_unit_id")
  
  staging_update <- staging_update %>%
    select(any_of(c(required_cols, optional_cols))) %>%
    mutate(point_type_confidence = 100,
           raw_unit_confidence = 100)  
  
  if(is.null(proceed)){
    proceed = askYesNo(msg = sprintf(
      "Do you want to proceed updating %s points for building %s",
                       nrow(staging_update),
                       building_info$name))
  }
  
  # Stop if not confirmed
  if (is.na(proceed) || !proceed) {
    stop("Operation canceled by user.")
  }
  
  #Convert to json
  staging_json <- staging_update %>% 
    #group points assigned to multiple equipment together
    group_by(across(-equip_names)) %>% 
    reframe(equip_names=list(equip_names))  %>%  
    #Convert points with multiple equip_names into a list
    mutate(across(equip_names, ~ (map(., function(x) (str_split(x, ", ")))))) %>% 
    #Convert NULL characters into NA
    mutate(across(everything(.), ~ ifelse(. == "NULL", NA, .))) %>% 
    split(1:nrow(.)) %>% 
    purrr::map(~ {
      row_list = as.list(.)
      
      topic = row_list$topic
      
      point_type = row_list[grepl("^point_type_", names(row_list))]
      names(point_type) = sub("point_type_","",names(point_type))
      
      raw_unit = row_list[grepl("^raw_unit_", names(row_list))]
      names(raw_unit) = sub("raw_unit_","",names(raw_unit))
      
      equip_names = unlist(row_list$equip_names, recursive = FALSE)
      
      # Build final structure dynamically and remove empty elements
      result <- list(
        topic = topic,
        point_type = if (is.null(point_type$tag_name) || is.na(point_type$tag_name)) NULL else point_type,
        raw_unit = if (is.null(raw_unit$id) || is.na(raw_unit$id)) NULL else raw_unit,
        equip_names = if(all(is.na(equip_names))) NA else equip_names
      )
      purrr::compact(result)  # Remove NULL elements
    }) %>%  
    unname()   %>% 
    toJSON(auto_unbox = TRUE, pretty = TRUE) 
  
  staging_json <- gsub("null","[]",staging_json)
  
  
  patch_output = api.patch(endpoint = paste0("staging/",building_info$id,"/points"),
                           json_body = staging_json)
  
  return(patch_output)
  
}

#' Update Staging Equipment
#'
#' Update equipment on the staging area.
#'
#' @param building Character vector or integer corresponding to the building name or id.
#'
#' @param staging_update A data.frame to upload to the staging area. Must contain name (equip_name) column. equipment_type_tag_name & new_name are optional columns
#'
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
#'
#' @return Result output of the update
#'
#'@export
update_staging_equipment <- function(building,
                                 staging_update, 
                                 proceed = NULL, verbose = TRUE){
  
  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")
  
  # Get building info
  building_info <- search_buildings(buildings = building, verbose = verbose)
  
  required_cols <- c("name")
  
  staging_update_cols <- names(staging_update)
  
  if(!all(required_cols %in% staging_update_cols)){
    stop(sprintf(
      "staging_update is missing cols %s",
      paste(required_cols, collapse = " or ")
    ))
  }
  
  if(is.null(proceed)){
    proceed = askYesNo(msg = sprintf(
      "Do you want to proceed updating %s equipment for building %s",
      nrow(staging_update),
      building_info$name))
  }
  
  # Stop if not confirmed
  if (is.na(proceed) || !proceed) {
    stop("Operation canceled by user.")
  }
  
  #Select Columns
  optional_cols <- c("equipment_type_tag_name","new_name")
  
  staging_update <- staging_update %>%
    select(any_of(c(required_cols, optional_cols))) %>%
    mutate(equipment_type_confidence = 100)  
  
  #Convert to JSON
  staging_json <- staging_update %>% 
    #Convert NULL characters into NA
    mutate(across(everything(.), ~ ifelse(. == "NULL", NA, .))) %>% 
    split(1:nrow(.))  %>% 
    purrr::map(~{
      row_list = as.list(.)
      
      #  row_list = staging_json[[1]]
      
      name = row_list$name
      
      equipment_type = row_list[grepl("^equipment_type_",names(row_list))]
      names(equipment_type) = sub("equipment_type_","",names(equipment_type))
      
      new_name = row_list$new_name
      
      result = list(name = name,equipment_type = equipment_type, new_name = new_name)
      
      # Build final structure dynamically and remove empty elements
      result <- list(
        name = name,
        equipment_type =
          if (is.null(equipment_type$tag_name) || is.na(equipment_type$tag_name))
            NULL else equipment_type,
        new_name = if(is.na(new_name)) NULL else new_name
      )
      purrr::compact(result)  # Remove NULL elements
    }) %>% 
    unname() %>%
    toJSON(auto_unbox = TRUE, pretty = TRUE) 
  
  
  patch_output = api.patch(endpoint = paste0("staging/",building_info$id,"/equipment"),
                           json_body = staging_json)
  
  return(patch_output)
}


##Update data on the staging area

#' Update Staging
#'
#' Update data on the staging area.
#'
#' @param building Character vector or integer corresponding to the building name or id. If you enter multiple building ids or names, only the first entry will be considered.
#'
#' @param staging_data A data.frame to upload to the staging area. Must contain e.equip_id and p.topic columns.
#'
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
#'
#' @return A named list containing any errors that may have occured during data upload.
#'
#'@export
upload_staging <- function(building,
                           staging_data,
                           move_topics = FALSE,
                           proceed = NULL,
                           verbose = TRUE) {
  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")
  
  # Get building info
  building_info <- search_buildings(buildings = building, verbose = verbose)
  
  # Validate `staging_data`
  if (missing(staging_data) || nrow(staging_data) == 0) {
    stop(
      "`staging_data` is missing or empty. It should be a data.frame containing at least 'e.equip_id' and/or 'p.topic' for the upload to succeed."
    )
  }
  
  if (!any(c("p.topic", "e.equip_id") %in% colnames(staging_data))) {
    stop("`staging_data` must include at least 'p.topic' or 'e.equip_id' as a column.")
  }
  
  if ('p.topic' %in% names(staging_data)) {
    if (move_topics == TRUE) {
      #This moves topics to the new equip_id/s by enforcing a list of equip_ids
      # Group by p.topic and combine e.equip_id into a list of unique values
      staging_data <- staging_data %>%
        group_by(p.topic) %>%
        summarise(e.equip_id = list(unique(e.equip_id))) %>%
        ungroup()
    }
  }
  
  # Convert data to JSON
  staging_data_json <- toJSON(staging_data)
  
  # Confirm upload if `proceed` is NULL
  if (is.null(proceed)) {
    proceed <- askYesNo(
      sprintf(
        "Do you want to proceed with uploading modified data for building '%s'?",
        building_info$name
      )
    )
  }
  
  # Stop if not confirmed
  if (is.na(proceed) || !proceed) {
    stop("Operation canceled by user.")
  }
  
  if (verbose) {
    cat("Uploading data to staging...\n")
  }
  
  # Construct endpoint URL
  endpoint <- paste0("staging/", building_info$id)
  
  # Make POST request
  response <- api.post(endpoint, json_body = staging_data_json)
  
  # Check response for errors
  row_errors <- response$row_errors
  
  # Provide feedback
  if (length(row_errors) == 0) {
    message <- "Upload successful!\n"
  } else {
    message <- "Upload unsuccessful. Check the returned errors for details.\n"
  }
  
  if (verbose) {
    cat(message)
  }
  
  # Return row errors if any
  if (length(row_errors) != 0) {
    return(row_errors)
  }
}

# Promote -----------------------------------------------------

#' Promote data on Staging Area
#'
#' Promote valid data on the staging area to the live building.
#'
#' @param building Character vector or integer corresponding to the building name or id.
#'
#' @param equip_ids_list  A list of all equip_ids to promote from staging to the live building
#'
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
#'
#' @return (Conditional) A named list with result output of promotion.
#'
#' @export

promote <- function(building,
                    equip_ids_list = NULL,
                    proceed = NULL,
                    verbose = TRUE) {
  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")
  
  building_info <- search_buildings(buildings = building, verbose = verbose)
  
  if (is.null(equip_ids_list)) {
    stop(sprintf(
      'Please provide a list of equip_ids to promote at %s?',
      building_info$name
    ))
  }
  
  equip_ids <- equip_ids_list$equip_ids
    
  if(is.null(proceed)){
  proceed <- askYesNo(
    sprintf(
      "Do you want to proceed promoting %s equip_ids at building %s:\n",
      length(equip_ids),
      building_info$name
    )
  )
  }  
    if (proceed == TRUE) {
      print("Promoting...")
    } else {
      stop('Stopping Operation.')
    }
  
equip_ids_list$topics = list()  

promote_json <- equip_ids_list %>% jsonlite::toJSON()

  # API call
  endpoint <- paste0("staging/", building_info$id, "/apply")

  result <- api.post(endpoint, json_body = promote_json)
  
  return(result)
  
}

# Demote ---------------------------------------------------------------


#' Demote Data from the live Building
#'
#' @param building Character vector or integer corresponding to the building name or id. If you enter multiple building ids or names, only the first entry will be considered.
#'
#' @param equipment_ids NULL (default) or A vector of equipment_ids to demote. Provide atleast one of equipment_ids or point_ids
#'
#' @param point_ids NULL(default) A vector of equipment_ids to demoted. Provide atleast one of equipment_ids or point_ids
#'
#' @param point_equipment_relationships NULL(default) A data.frame containing equipment_id and point_id relationships to demote
#'
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
#'
#' @return (Conditional) A named list containing errors that may have occurred during data promotion.
#'
#' @export
demote <- function(building,
                   equipment_ids = NULL,
                   point_ids = NULL,
                   point_equipment_relationships = NULL,
                   proceed = NULL,
                   verbose = TRUE) {
  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")
  
  # Check if at least one of the necessary parameters is provided
  if (is.null(equipment_ids) &&
      is.null(point_ids) &&
      is.null(point_equipment_relationships)) {
    stop(
      'Please provide at least one of the equipment_ids, point_ids, or point_equipment_relationships to demote.'
    )
  }
  
  
  # Get building info and ensure relationships are available
  building_info <- search_buildings(buildings = building, verbose = verbose)
  
  if (is.null(point_equipment_relationships)) {
    confirm_metadata_pull <- askYesNo(
      "You have not provided explicit point-equipment relationships to demote. \nDo you want to remove all existing point-equipment relationships for the given points? \n"
    )
    
    if (is.na(confirm_metadata_pull) |
        confirm_metadata_pull != TRUE) {
      stop('Stopping Operation.\n')
    }
    
    # Fetch point-equipment relationships if not provided
    metadata <- get_metadata(buildings = building_info$id,verbose = FALSE)
    
    point_equipment_relationships <- metadata %>%
      filter(e.equipment_id %in% equipment_ids |
               p.point_id %in% point_ids) %>%
      select(equipment_id = e.equipment_id, point_id = p.point_id)
  }
  
  # Prepare the demotion message
  unpromote_message <- sprintf(
    "Proceed with demotion on %s:\n%s equipment \n%s points \n%s equipment-point relationships",
    building_info$name,
    length(equipment_ids),
    length(point_ids),
    nrow(point_equipment_relationships)
  )
  
  # Prompt for confirmation
  if (is.null(proceed)) {
    proceed <- askYesNo(unpromote_message)
  }
  
  if (is.na(proceed) | proceed != TRUE) {
    stop('Stopping Operation.\n')
  }
  
  # Default to empty lists if arguments are NULL
  if (is.null(equipment_ids)) {
    equipment_ids = 0
  }
  
  if (is.null(point_ids)) {
    point_ids = 0
  }
  
  # Create a list for the demotion payload
  unpromote_list <- list(
    equipment_ids = equipment_ids,
    point_ids = point_ids,
    point_equipment_relationships = point_equipment_relationships
  )
  
  # Convert the payload to JSON
  unpromote_json <- toJSON(unpromote_list)
  
  # Define the endpoint
  endpoint <- paste0('staging/', building_info$id, '/apply')
  
  # Send the delete request
  api.delete(endpoint, json_body = unpromote_json)
  
}