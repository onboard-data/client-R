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
#' @noRd
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

#' Get Staging Data
#' Retrieve metadata (points, equipment, devices) from the staging area for one or more buildings.
#' @inheritParams buildings
#' @inheritParams verbose
#' @return A data.frame containing combined metadata from the staging area.
#' @export
get_staging_data <- function(buildings, verbose = TRUE) {

  buildings <- search_buildings(buildings = buildings, verbose = verbose)

  # Fetch and join points/equipment/devices for each building
  building_staging_data <- purrr::map2(buildings$id, buildings$name, function(bldg_id, bldg_name) {

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
  })

  # Combine all buildings in a single pass (rbind.fill accepts a list
  # directly), instead of growing the data.frame one rbind at a time
  staging_data <- plyr::rbind.fill(building_staging_data)

  # Drop rows where device_id and topic are both missing
  staging_data <- staging_data %>%
    filter(!((is.na(d.device_id) | d.device_id == "NULL") &
             (is.na(p.topic) | p.topic == "NULL")))

  # Cleanup
  staging_data <- staging_data %>%
    select(-contains(c("state_text", "@prop")))

  # Fetch data-model lookups once (not per building)
  point_types <- api.request("pointtypes", verbose = FALSE)
  units <- api.request("unit", verbose = FALSE)
  equipment_types <- api.request("equiptype", verbose = FALSE)

  staging_data_final <- staging_data %>%
    left_join(select(equipment_types, id, e.equipment_type_tag_name = tag_name),
              by = c("e.equipment_type_id" = "id")) %>%
    left_join(select(point_types, id, p.point_type_tag_name = tag_name),
              by = c("p.point_type_id" = "id")) %>%
    left_join(select(units, id, p.raw_unit = name_abbr),
              by = c("p.raw_unit_id" = "id")) %>%
    select(order(colnames(.)))

  if (verbose) cat("Staging data created.\n")

  staging_data_final
}


# Update Staging -------------------------------------------------------
#
# NOTE: .resolve_single_building(), .confirm_or_stop(), .require_cols(),
# .default_confidence_col(), .extract_prefixed_fields(), .nullify_null_strings(),
# and .as_list_if_scalar() used below now live in helpers.R -- they're generic
# utilities, not staging-specific, and .as_list_if_scalar() in particular is
# also used from buildings.R and timeseries.R.

#' Update Staging Points
#'
#' Update points on the staging area.
#'
#' @inheritParams building
#'
#' @param staging_points A data.frame to upload to the staging area. Must contain equip_ids and topic columns. point_type_tag_name, point_type_confidence and raw_unit are optional columns
#'
#' @inheritParams proceed
#' @return Result output of the update
#' 
#'
#' @examples
#' \dontrun{
#' staging_points <- data.frame(equip_ids=c("f6496c46-b2d1-439d-9957-9c26f2025566","3229474_49"), topic="onboard/sandbox/47808/3229474/analogInput/2")
#'
#' update_staging_points( building = "Sandbox",staging_points = staging_points)
#' }
#'
#'
#'@export
update_staging_points <- function(building,
                                  staging_points,
                                  proceed = NULL,verbose = TRUE){

  building_info <- .resolve_single_building(building, verbose)

  required_cols <- c("topic")

  staging_points_cols <- names(staging_points)

  .require_cols(staging_points_cols, required_cols, "staging_points")

  if(('raw_unit' %in% staging_points_cols)){
    #Getting unit ids to match
    units <- api.request(endpoint = "unit",verbose = FALSE) %>%
      select(raw_unit = name_abbr,raw_unit_id = id)

    staging_points <- left_join(staging_points,units, by =c("raw_unit"))
  }

  #Select columns
  optional_cols <- c("equip_ids","point_type_tag_name","point_type_confidence","raw_unit_id")

  staging_points <- staging_points %>%
    select(any_of(c(required_cols, optional_cols))) %>%
    .default_confidence_col("point_type_confidence") %>%
    .default_confidence_col("raw_unit_confidence")

  .confirm_or_stop(proceed, sprintf(
    "Do you want to proceed updating %s points for building %s",
    nrow(staging_points),
    building_info$name))

  if("equip_ids" %in% staging_points_cols ){
  staging_points <- staging_points %>%
  #group points assigned to multiple equipment together
  group_by(across(-equip_ids)) %>%
    reframe(equip_ids=list(equip_ids))  %>%
    #Convert points with multiple equip_ids into a list
    mutate(across(equip_ids, ~ (purrr::map(., function(x) (stringr::str_split(x, ", "))))))
  remove_equip_ids = FALSE
  } else{
  remove_equip_ids = TRUE
  }

  #Convert body
  staging_body <- staging_points %>%
    .nullify_null_strings() %>%
    distinct(topic,.keep_all = TRUE) %>%
    split(1:nrow(.)) %>%
    purrr::map(~ {
      row_list = as.list(.)

      topic = row_list$topic

      point_type = .extract_prefixed_fields(row_list, "point_type_")
      raw_unit = .extract_prefixed_fields(row_list, "raw_unit_")

      equip_ids = unlist(row_list$equip_ids, recursive = FALSE) 

      # Build final structure dynamically and remove empty elements
      result <- list(
        topic = topic,
        point_type = if (is.null(point_type$tag_name) || is.na(point_type$tag_name)) NULL else point_type,
        raw_unit = if (is.null(raw_unit$id) || is.na(raw_unit$id)) NULL else raw_unit,
        equip_ids = if(all(is.na(equip_ids)) || all(equip_ids =="")) NA
        else equip_ids
      )
      purrr::compact(result)  # Remove NULL elements
    })  %>%
    unname()

  if(remove_equip_ids==TRUE){
    staging_body <-lapply(staging_body,function(x){
      x$equip_ids <- NULL
      x})
  } else {
    #Convert na equip_ids to empty list
    staging_body <- lapply(staging_body, function(x) {
      if (all(is.na(x$equip_ids))) x$equip_ids <- list()
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

  building_info <- .resolve_single_building(building, verbose)

  required_cols <- c("name")

  staging_equip_cols <- names(staging_equip)

  .require_cols(staging_equip_cols, required_cols, "staging_equip")

  .confirm_or_stop(proceed, sprintf(
    "Do you want to proceed updating %s equipment for building %s",
    nrow(staging_equip),
    building_info$name))

  #Select Columns
  optional_cols <- c("equipment_type_tag_name","equipment_type_confidence","new_name")

  staging_equip <- staging_equip %>%
    select(any_of(c(required_cols, optional_cols))) %>%
    .default_confidence_col("equipment_type_confidence")

  #Convert body
  staging_body <- staging_equip %>%
    .nullify_null_strings() %>%
    split(1:nrow(.))  %>%
    purrr::map(~{
      row_list = as.list(.)

      name = row_list$name

      equipment_type = .extract_prefixed_fields(row_list, "equipment_type_")

      new_name = row_list$new_name

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

  building_info <- .resolve_single_building(building, verbose)

  if (is.null(equipment)) {
    stop(sprintf(
      'Please provide equip_ids to publish at %s?',
      building_info$name
    ))
  }

  publish_list <- list(
    equip_ids = .as_list_if_scalar(equipment),
    topics = if (is.null(topics)) list() else .as_list_if_scalar(topics)
  )

  #publish_list %>% toJSON(auto_unbox = TRUE,pretty = TRUE)

  .confirm_or_stop(proceed, sprintf(
    "Do you want to proceed publishing %s equip_ids at building %s:\n",
    length(equipment),
    building_info$name
  ))

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
  # NOTE: kept as an explicit inline check (rather than going through
  # .resolve_single_building()) so this validation still runs *before* the
  # "at least one param" check below, matching the original error-ordering
  # and avoiding an unnecessary search_buildings() API call when that check
  # is the one that ends up failing.
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
  building_info <- .resolve_single_building(building, verbose)

  # Prepare the demotion message
  unpublish_message <- sprintf(
    "Proceed with unplishing on %s:\n%s equipment \n%s points \n%s equipment-point relationships",
    building_info$name,
    ifelse(equipment_ids == 0,0,length(equipment_ids)),
    length(point_ids),
    nrow(point_equipment_relationships)
  )

  .confirm_or_stop(proceed, unpublish_message)

  # Default to empty lists if arguments are NULL
  if (is.null(equipment_ids)) {
    equipment_ids = list(0)
  }

  if (is.null(point_ids)) {
    point_ids = list(0)
  }

  equipment_ids <- .as_list_if_scalar(equipment_ids)
  point_ids <- .as_list_if_scalar(point_ids)

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
