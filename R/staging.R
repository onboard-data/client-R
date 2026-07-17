# Internal helpers --------------------------------------------------------

#' Fetch and Normalize a Staging Resource
#'
#' Internal helper shared by `get_staging_equipment()`, `get_staging_devices()`,
#' and `get_staging_points()`. Fetches a staging sub-resource for a building,
#' flattens the JSON response into a data.frame, converts known timestamp
#' columns, and prefixes every column name so the resource can be safely
#' joined with the others later on.
#'
#' @param building_id Integer. ID of the building.
#' @param resource Character. Staging sub-resource path segment, e.g.
#'   `"equipment"`, `"devices"`, or `"points"`.
#' @param prefix Character. Prefix to prepend to every column name (e.g. `"e."`).
#' @param placeholder A one-row data.frame to return (with `prefix` applied)
#'   when the API response is empty. Column names/values match what each
#'   original caller used, so behavior for empty responses is unchanged.
#' @param response_body Passed through to `api.request()`. Defaults to `"string"`.
#' @inheritParams verbose
#'
#' @return A data.frame of the requested staging resource, with column names prefixed.
.get_staging_resource <- function(building_id,
                                   resource,
                                   prefix,
                                   placeholder,
                                   verbose = TRUE,
                                   response_body = "string") {

  data <- api.request(
    endpoint = paste0("staging/", building_id, "/", resource),
    verbose = verbose,
    response_body = response_body
  )

  if (length(data) == 0) {
    if (verbose) cat(sprintf("No %s found...\n", resource))
    data <- placeholder
  } else {
    data <- jsonlite::fromJSON(jsonlite::toJSON(data), flatten = TRUE) %>%
      convert_to_datetime()
  }

  names(data) <- paste0(prefix, names(data))

  data
}

# Equipment ----------------------------------------------------

#' Get Staging Equipment
#' Retrieve all equipment from the staging area for a building.
#' @param building_id Integer. ID of the building.
#' @inheritParams verbose
#' @return A data.frame of staging equipment.
#' @export
get_staging_equipment <- function(building_id, verbose = TRUE) {
  .get_staging_resource(
    building_id = building_id,
    resource = "equipment",
    prefix = "e.",
    placeholder = data.frame(equip_id = NA, equipment_type_id = NA),
    verbose = verbose
  )
}

# Devices ------------------------------------------------------

#' Get Staging Devices
#' Retrieve all devices from the staging area for a building.
#' @inheritParams get_staging_equipment
#' @return A data.frame of staging devices.
#' @export
get_staging_devices <- function(building_id, verbose = TRUE) {
  .get_staging_resource(
    building_id = building_id,
    resource = "devices",
    prefix = "d.",
    placeholder = data.frame(device_id = NA, staging_id = NA, building_id = NA),
    verbose = verbose
  )
}

# Points -------------------------------------------------------

#' Get Staging Points
#' Retrieve all points from the staging area for a building.
#' @inheritParams get_staging_equipment
#' @return A data.frame of staging points.
#' @export
get_staging_points <- function(building_id, verbose = TRUE) {

  points <- .get_staging_resource(
    building_id = building_id,
    resource = "points",
    prefix = "p.",
    placeholder = data.frame(equip_ids = NA, staging_device_id = NA, point_type_id = NA, raw_unit_id = NA, topic = NA),
    verbose = verbose,
    response_body = "json"
  )

  points %>%
    mutate(across(where(is.list), ~ sapply(., toString))) %>% # Convert list elements into character
    separate_rows(p.equip_ids, sep = ",\\s*") %>% # Split by comma and optional space
    mutate(p.equip_ids = as.character(p.equip_ids))
}

# Combined Data --------------------------------------------------------

#' Fetch Combined Staging Data for a Single Building
#'
#' Internal helper for `get_staging_data()`. Fetches staged points, equipment,
#' and devices for one building and joins them into a single data.frame.
#'
#' @param bldg_id Integer. Building ID.
#' @param bldg_name Character. Building name (used for verbose messages and
#'   the resulting `building_name` column).
#' @inheritParams verbose
#'
#' @return A data.frame combining staged points, equipment, and devices for the building.
.fetch_building_staging_data <- function(bldg_id, bldg_name, verbose = TRUE) {

  if (verbose) cat(sprintf("Fetching staging data for %s...\n", bldg_name))

  if (verbose) cat("Fetching staged points...\n")
  points <- get_staging_points(building_id = bldg_id, verbose = FALSE)

  if (verbose) cat("Fetching staged equipment...\n")
  equip <- get_staging_equipment(building_id = bldg_id, verbose = FALSE)

  if (verbose) cat("Fetching staged devices...\n")
  devices <- get_staging_devices(building_id = bldg_id, verbose = FALSE) %>%
    mutate(building_name = bldg_name)

  points %>%
    full_join(equip, by = c("p.equip_ids" = "e.equip_id")) %>%
    mutate(across(c("p.staging_device_id", ends_with("type_id"), ends_with("unit_id")),
                  ~ as.integer(.))) %>%
    full_join(devices, by = c("p.staging_device_id" = "d.staging_id")) %>%
    rename(building_id = d.building_id) %>%
    distinct()
}

#' Enrich Staging Data with Type/Unit Labels
#'
#' Internal helper for `get_staging_data()`. Joins raw staging metadata against
#' the data model (equipment types, point types, units) to attach
#' human-readable labels, and sorts columns alphabetically.
#'
#' @param staging_data A data.frame produced by combining staged points, equipment, and devices.
#'
#' @return `staging_data` with `e.equipment_type_tag_name`, `p.point_type_tag_name`,
#'   and `p.raw_unit` columns added, columns sorted alphabetically.
.enrich_staging_metadata <- function(staging_data) {

  point_types <- api.request("pointtypes", verbose = FALSE)
  units <- api.request("unit", verbose = FALSE)
  equipment_types <- api.request("equiptype", verbose = FALSE)

  staging_data %>%
    left_join(select(equipment_types, id, e.equipment_type_tag_name = tag_name),
              by = c("e.equipment_type_id" = "id")) %>%
    left_join(select(point_types, id, p.point_type_tag_name = tag_name),
              by = c("p.point_type_id" = "id")) %>%
    left_join(select(units, id, p.raw_unit = name_abbr),
              by = c("p.raw_unit_id" = "id")) %>%
    select(order(colnames(.)))
}

#' Get Staging Data
#' Retrieve metadata (points, equipment, devices) from the staging area for one or more buildings.
#' @inheritParams buildings
#' @inheritParams verbose
#' @return A data.frame containing combined metadata from the staging area.
#' @export
get_staging_data <- function(buildings, verbose = TRUE) {

  buildings <- search_buildings(buildings = buildings, verbose = verbose)

  staging_data <- purrr::map2(
    buildings$id, buildings$name,
    ~ .fetch_building_staging_data(bldg_id = .x, bldg_name = .y, verbose = verbose)
  ) %>%
    purrr::reduce(plyr::rbind.fill, .init = data.frame())

  # Drop rows where device_id and topic are both missing
  staging_data <- staging_data %>%
    filter(!((is.na(d.device_id) | d.device_id == "NULL") &
             (is.na(p.topic) | p.topic == "NULL")))

  # Cleanup
  staging_data <- staging_data %>%
    select(-contains(c("state_text", "@prop")))

  staging_data_final <- .enrich_staging_metadata(staging_data)

  if (verbose) cat("Staging data created.\n")

  staging_data_final
}


# Update Staging -------------------------------------------------------

#' Update Staging Points
#'
#' Update points on the staging area.
#'
#' @inheritParams building
#'
#' @param staging_points A data.frame to upload to the staging area. Must contain equip_names and topic columns. point_type_tag_name, point_type_confidence and raw_unit are optional columns
#'
#' @inheritParams proceed
#' @return Result output of the update
#'
#'@export
update_staging_points <- function(building,
                                  staging_points,
                                  proceed = NULL,verbose = TRUE){

  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")

  # Get building info
  building_info <- search_buildings(buildings = building, verbose = verbose)

  required_cols <- c("topic")

  staging_points_cols <- names(staging_points)

  if(!all(required_cols %in% staging_points_cols)){
    stop(sprintf(
      "staging_points is missing cols %s",
      paste(required_cols, collapse = " or ")
    ))
  }

  if(('raw_unit' %in% staging_points_cols)){
    #Getting unit ids to match
    units <- api.request(endpoint = "unit",verbose = FALSE) %>%
      select(raw_unit = name_abbr,raw_unit_id = id)

    staging_points <- left_join(staging_points,units, by =c("raw_unit"))
  }

  #Select columns
  optional_cols <- c("equip_names","point_type_tag_name","point_type_confidence","raw_unit_id")

  staging_points <- staging_points %>%
    select(any_of(c(required_cols, optional_cols))) %>%
    {
      if (!"point_type_confidence" %in% names(.))
        mutate(., point_type_confidence = 100)
      else
        .
    } %>%
    mutate(raw_unit_confidence = 100)

  if(is.null(proceed)){
    proceed = askYesNo(msg = sprintf(
      "Do you want to proceed updating %s points for building %s",
                       nrow(staging_points),
                       building_info$name))
  }

  # Stop if not confirmed
  if (is.na(proceed) || !proceed) {
    stop("Operation canceled by user.")
  }

  if("equip_names" %in% staging_points_cols ){
  staging_points <- staging_points %>%
  #group points assigned to multiple equipment together
  group_by(across(-equip_names)) %>%
    reframe(equip_names=list(equip_names))  %>%
    #Convert points with multiple equip_names into a list
    mutate(across(equip_names, ~ (purrr::map(., function(x) (stringr::str_split(x, ", "))))))
  remove_equip_names = FALSE
  } else{
  remove_equip_names = TRUE
  }

  #Convert body
  staging_body <- staging_points %>%
    #Convert NULL characters into NA
    mutate(across(everything(.), ~ ifelse(. == "NULL", NA, .))) %>%
    distinct(topic,.keep_all = TRUE) %>%
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
        equip_names = if(all(is.na(equip_names)) || all(equip_names =="")) NA
        else equip_names
      )
      purrr::compact(result)  # Remove NULL elements
    })  %>%
    unname()

  if(remove_equip_names==TRUE){
    staging_body <-lapply(staging_body,function(x){
      x$equip_names <- NULL
      x})
  } else {
    #COnvert na equip_names to empty list
    staging_body <- lapply(staging_body, function(x) {
      if (all(is.na(x$equip_names))) x$equip_names <- list()
      x
    })
  }

  #Check json body (for debugging)
  staging_body %>% jsonlite::toJSON(auto_unbox = TRUE,pretty = TRUE)

  api_output = api.request(endpoint = paste0("staging/",building_info$id,"/points"),
                           method = "PATCH",
                           request_body = staging_body)

  return(api_output)

}

#' Update Staging Equip
#'
#' Update equipment on the staging area.
#'
#' @inheritParams building
#'
#' @param staging_equip A data.frame to upload to the staging area. Must contain name (equip_name) column. equipment_type_tag_name, equipment_type_confidence & new_name are optional columns
#'
#' @inheritParams proceed
#'
#' @return Result output of the update
#'
#'@export
update_staging_equip <- function(building,
                                 staging_equip,
                                 proceed = NULL, verbose = TRUE){

  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")

  # Get building info
  building_info <- search_buildings(buildings = building, verbose = verbose)

  required_cols <- c("name")

  staging_equip_cols <- names(staging_equip)

  if(!all(required_cols %in% staging_equip_cols)){
    stop(sprintf(
      "staging_equip is missing cols %s",
      paste(required_cols, collapse = " or ")
    ))
  }

  if(is.null(proceed)){
    proceed = askYesNo(msg = sprintf(
      "Do you want to proceed updating %s equipment for building %s",
      nrow(staging_equip),
      building_info$name))
  }

  # Stop if not confirmed
  if (is.na(proceed) || !proceed) {
    stop("Operation canceled by user.")
  }

  #Select Columns
  optional_cols <- c("equipment_type_tag_name","equipment_type_confidence","new_name")

  staging_equip <- staging_equip %>%
    select(any_of(c(required_cols, optional_cols))) %>%
    {
      if (!"equipment_type_confidence" %in% names(.))
        mutate(., equipment_type_confidence = 100)
      else
        .
    }

  #Convert body
  staging_body <- staging_equip %>%
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
        new_name = if(is.null(new_name) || is.na(new_name)) NULL else new_name
      )
      purrr::compact(result)  # Remove NULL elements
    }) %>%
    unname()


  api_output = api.request(endpoint = paste0("staging/",building_info$id,"/equipment"),
                           method = "PATCH",
                           request_body = staging_body)

  return(api_output)
}

# Publish -----------------------------------------------------

#' Publish data on Staging Area
#'
#' Publish valid data on the staging area to the live building.
#'
#' @inheritParams building
#'
#' @param equipment  A vector containing all equip_ids to Publish from staging to the live building
#'
#' @param topics (Optional) A vector containing all topics to publish for the corresponding equip_ids
#'
#' @inheritParams proceed
#'
#' @return (Conditional) A named list with result output of promotion.
#'
#' @export
publish <- function(building,
                    equipment = NULL,
                    topics = NULL,
                    proceed = NULL,
                    verbose = TRUE) {
  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")

  building_info <- search_buildings(buildings = building, verbose = verbose)

  if (is.null(equipment)) {
    stop(sprintf(
      'Please provide equip_ids to publish at %s?',
      building_info$name
    ))
  }

  publish_list <- list()

  #Set Equip IDs
  if (length(equipment) == 1) {
    publish_list$equip_ids = list(equipment)
  } else {
    publish_list$equip_ids = equipment
  }

  #Set Topics
  if (is.null(topics)) {
    publish_list$topics = list()
  } else {
    if (length(topics) == 1) {
      publish_list$topics = list(topics)
    } else {
      publish_list$topics = topics
    }
  }

  #publish_list %>% toJSON(auto_unbox = TRUE,pretty = TRUE)

  if(is.null(proceed)){
  proceed <- askYesNo(
    sprintf(
      "Do you want to proceed publishing %s equip_ids at building %s:\n",
      length(equipment),
      building_info$name
    )
  )
  }
    if (is.na(proceed)| proceed != TRUE) {

      stop('Stopping Operation.')
    }

  # API call
  endpoint <- paste0("staging/", building_info$id, "/apply")

  api_output <- api.request(endpoint,
                            method =  "POST",
                            request_body = publish_list)

  return(api_output)

}

# Unpublish ---------------------------------------------------------------


#' Unpublish Data from the live Building
#'
#' @inheritParams building
#'
#' @param equipment_ids NULL (default) or A vector of equipment_ids to unpublish. Provide atleast one of equipment_ids or point_ids
#'
#' @param point_ids NULL(default) A vector of equipment_ids to unpublishd. Provide atleast one of equipment_ids or point_ids
#'
#' @param point_equipment_relationships NULL(default) A data.frame containing equipment_id and point_id relationships to unpublish
#'
#' @inheritParams proceed
#'
#' @return (Conditional) A named list containing errors that may have occurred during data promotion.
#'
#' @export
unpublish <- function(building,
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
      'Please provide at least one of the equipment_ids, point_ids, or point_equipment_relationships to unpublish.'
    )
  }

  # Get building info and ensure relationships are available
  building_info <- search_buildings(buildings = building, verbose = verbose)

  # Prepare the demotion message
  unpublish_message <- sprintf(
    "Proceed with unplishing on %s:\n%s equipment \n%s points \n%s equipment-point relationships",
    building_info$name,
    ifelse(equipment_ids == 0,0,length(equipment_ids)),
    length(point_ids),
    nrow(point_equipment_relationships)
  )

  # Prompt for confirmation
  if (is.null(proceed)) {
    proceed <- askYesNo(unpublish_message)
  }

  if (is.na(proceed) | proceed != TRUE) {
    stop('Stopping Operation.\n')
  }

  # Default to empty lists if arguments are NULL
  if (is.null(equipment_ids)) {
    equipment_ids = list(0)
  }

  if (is.null(point_ids)) {
    point_ids = list(0)
  }

  if(length(equipment_ids) == 1){
    equipment_ids = list(equipment_ids)
  }

  if(length(point_ids) == 1){
    point_ids = list(point_ids)
  }

  # Create a list for the unpublish payload
  unpublish_list <- list(
    equipment_ids = equipment_ids,
    point_ids = point_ids,
    point_equipment_relationships = point_equipment_relationships
  )

  #check
  unpublish_list %>% toJSON(pretty = T,auto_unbox = T)

  # Send the delete request
  api.request(endpoint = paste0('staging/', building_info$id, '/apply'),
              method = "DELETE",
              request_body = unpublish_list)

  }

