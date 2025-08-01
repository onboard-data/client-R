# Buildings Database Search --------------------------------------------------------

#' Search Buildings in Your Organization
#'
#' Search buildings in your organization by ID or name.
#'
#' @inheritParams buildings
#' @inheritParams verbose
#'
#' @return A dataframe containing the matched building(s).
#'
#' @examples
#' \dontrun{
#' # Search by building IDs
#' search_buildings(buildings = c(427, 428))
#'
#' # Search by name
#' search_buildings(buildings = "Laboratory")
#'
#' # Mixed search
#' search_buildings(buildings = c(427, "Laboratory"))
#' }
#'
#' @export
search_buildings <- function(buildings = NULL,
                             verbose = TRUE) {
  if (is.null(buildings) || length(buildings) == 0) {
    stop("The 'buildings' parameter is required and cannot be empty.")
  }
  
  all_buildings <- api.request(endpoint = "buildings",
                               verbose = FALSE)

  if (is.numeric(buildings)) {
    result <- all_buildings %>%
      dplyr::filter(id %in% buildings)
    
  } else {
    search_text <- paste(buildings, collapse = "|")
    
    result <- all_buildings %>% 
      dplyr::filter(id %in% buildings | grepl(search_text, name, ignore.case = TRUE))
    
  }
  if (nrow(result) == 0) {
    stop("No buildings found. Please check your input.")
  }
  
  if (verbose) {
    cat(sprintf("Found %d building(s): %s\n", nrow(result), paste(result$name, collapse = ", ")))
  }
  
  return(result)
}

# Point Selector Search ----------------------------------------------------------

#' Point Selector Template
#'
#' Create a query template to select metadata points.
#' 
#' @returns An empty named list of possible point selection criteria.
#' 
#' @examples 
#' \dontrun{
#' query <- PointSelector()
#' 
#' query$buildings <- 101
#' query$equipment_types <- 'HVAC/AHU'
#' query$point_types <- c('supply_air_temperature_sensor','supply_air_static_pressure_sensor')
#' }
#' 
#' @export
PointSelector <- function(){
  query <- list(
    orgs = '',
    buildings = '',
    point_ids = '',
    point_names = '',
    point_topics = '',
    updated_since = '',
    point_types = '',
    equipment = '',
    equipment_types = ''
  )
  
  return(query)
}


#' Select Points Based on Query
#'
#' Use a query generated from `PointSelector()` to fetch matching point metadata via a POST API request.
#'
#' @param query A named list, typically created using `PointSelector()`, defining filter criteria such as buildings, point types, or equipment.
#' @inheritParams verbose
#' @return A named list of all the selected buildings, equipment IDs, equipment_types, point_types & point IDs for that category.
#'
#' @examples
#' \dontrun{
#' query <- PointSelector()
#' query$buildings <- 427
#' query$equipment_types <- 'HVAC/AHU'
#' query$point_types <- c('supply_air_temperature_sensor','supply_air_static_pressure_sensor')
#'
#' points <- select_points(query)
#' }
#'
#' @export
select_points <- function(query, verbose = TRUE){
  
  # Convert `updated_since` if specified
  if (!is.null(query$updated_since) && query$updated_since != "") {
    query$updated_since <- as.numeric(as.POSIXct(query$updated_since, tz = "UTC"))
  }
  
  # Drop empty values
  query <- query[query != ""]
  
  # Wrap each non-empty field in a list
  query <- lapply(query, as.list)
  
  # POST the query to the endpoint
  endpoint <- "points/select"
  response <- api.request(
    endpoint = endpoint,
    method = "POST",
    request_body = query,
    verbose = verbose
  )
  
  return(response)
}


# Points by ID ---------------------------------------------------------

#' Get Points by ID
#'
#' Queries point metadata by a list of point IDs.
#'
#' @inheritParams point_ids
#' @inheritParams verbose
#'
#' @return A `data.frame` containing metadata of the requested points. Returns an empty `data.frame` if no points are found.
#'
#' @examples
#' \dontrun{
#' points <- get_points_by_ids(c(10000, 10001))
#'
#' query <- PointSelector()
#' query$buildings <- 101
#' query$equipment_types <- 'HVAC/AHU'
#' query$point_types <- c('supply_air_temperature_sensor', 'supply_air_static_pressure_sensor')
#'
#' selection <- select_points(query)
#' points <- get_points_by_ids(selection$points)
#' }
#'
#' @export
get_points_by_ids <- function(point_ids, verbose = TRUE){
  
  if (length(point_ids) == 0) {
    warning("No point IDs provided.")
    return(data.frame())
  }
  
  id_unlist <- unlist(point_ids)
  
  #Separate point ids into chunks of 500
  chunks <- split(id_unlist,
                  ceiling(seq_along(id_unlist)/500))
  
  
  all_points <- data.frame()
  
  for (chunk in chunks) {
    encoded_ids <- URLencode(toJSON(chunk), reserved = TRUE)
    endpoint <- paste0("points?point_ids=", encoded_ids)
    
    points_chunk <- api.request(endpoint = endpoint, verbose = verbose)
    
    all_points <- plyr::rbind.fill(all_points, points_chunk)
  }
  
  return(all_points)
  
}


# Equipment by ID ---------------------------------------------------------

#' Get Equipment by ID
#'
#' Queries equipment metadata by equipment IDs.
#'
#' @inheritParams equipment_ids
#' @inheritParams verbose
#'
#' @return A `data.frame` containing metadata of the requested equipment. Returns an empty list if no matches are found.
#'
#' @examples
#' \dontrun{
#' equipment <- get_equipment_by_ids(c(1000, 1001))
#'
#' query <- PointSelector()
#' query$buildings <- 101
#' query$equipment_types <- 'HVAC/AHU'
#' selection <- select_points(query)
#'
#' equipment <- get_equipment_by_ids(selection$equipment)
#' }
#'
#' @export
get_equipment_by_ids <- function(equipment_ids, verbose = TRUE){
  
  if (length(equipment_ids) == 0) {
    warning("No equipment IDs provided.")
    return(data.frame())
  }
  
  request_body <- list(equipment_ids = equipment_ids)

  equipment <- api.request(
    endpoint = "equipment/query",
    method = "POST",
    request_body = request_body,
    verbose = verbose
  )
  
  return(equipment)
}

# Metadata ----------------------------------------------------------------

#' GET Metadata
#'
#' Retrieves live points and equipment for a given building or selection and outputs a clean metadata data.frame.
#'
#' @inheritParams buildings
#' @param selection Selection list from point selector.
#' @inheritParams verbose
#'  
#' @return A data.frame of clean metadata for the requested points.
#'
#' @examples
#' \dontrun{
#' metadata <- get_metadata(buildings = c(427, "Laboratory"))
#'
#' OR
#'
#' query <- PointSelector()
#' query$buildings <- 427
#' query$equipment_types <- 'HVAC/AHU'
#' query$point_types <- c('supply_air_temperature_sensor','supply_air_static_pressure_sensor')
#'
#' selection <- select_points(query)
#' metadata <- get_metadata(selection)
#' }
#' @export
get_metadata <- function(buildings = NULL,
                         selection = NULL,
                         verbose = TRUE) {
  
  if (is.null(selection) && is.null(buildings)) {
    stop("Provide either building names/IDs or a selection list.")
  }
  
  equip_data <- data.frame()
  points_data <- data.frame()
  
    if (!is.null(buildings)) {

    building_info <- search_buildings(buildings = buildings, verbose = verbose)
    
    for (i in seq_along(building_info$id)){
    
    bid <- building_info$id[i]
    bname <- building_info$name[i]
    
    if (verbose) cat(sprintf("Querying equipment & points for building: %s (id:%s)...\n", bname, bid))
    
    #Fetch equipment data
    equip_data <- plyr::rbind.fill(equip_data, api.request(paste0("buildings/", bid, "/equipment"), verbose = FALSE))
    
    #Fetch points data
    points_data_bldg <- api.request(paste0("buildings/", bid, "/points"), verbose = FALSE) 
    #Handle state_text columns if they exist
    if ("state_text" %in% names(points_data_bldg)) {
      points_data_bldg <- points_data_bldg %>% 
        rowwise() %>%
        mutate(state_text = paste(na.omit(unlist(state_text)), collapse = ", ")) %>%
        ungroup()
    }
    
    points_data <- plyr::rbind.fill(points_data, points_data_bldg)
      
    }
  } else {
    if (!is.list(selection) || is.atomic(selection)) {
      stop("Selection must be a non-atomic named list with fields like: equipment, points, etc.")
    }
    
    if (length(selection$points) == 0) stop("No metadata found.")
    
    if (verbose) cat(sprintf("Querying %s points...\n", length(selection$points)))
    points_data <- get_points_by_ids(selection$points, verbose = FALSE)
    
    if (verbose) cat(sprintf("Querying %s equipment...\n", length(selection$equipment)))
    equip_data <- get_equipment_by_ids(selection$equipment, verbose = FALSE)
  }
  
  # Normalize and separate rows by equip_id
    points_data <- points_data %>%
    mutate(across(equip_id, ~ gsub("c\\(|\\)", "", .))) %>% # Remove "c()" if present
    separate_rows(equip_id, sep = ",\\s*") %>%      # Split by comma and optional space
    mutate(across(equip_id, ~ suppressWarnings(as.numeric(.))))
  
    names(points_data) <- paste0("p.", names(points_data))
    names(equip_data) <- paste0("e.", names(equip_data))

  # Handle equipment relationships

    if ("e.source_equip" %in% names(equip_data)) {
      id_name_map <- equip_data %>% select(e.id, e.name)
  
      source_equip <- equip_data %>%
        select(e.id, starts_with("e.source_equip")) %>%
        pivot_longer(cols = -e.id, names_to = "source", values_to = "rel") %>%
        filter(rel != "NULL") %>%
        mutate(source = as.integer(gsub("e.source_equip\\.", "", source))) %>%
        left_join(id_name_map, by = c("source" = "e.id")) %>%
        select(e.id, source = e.name)

      target_equip <- equip_data %>%
        select(e.id, starts_with("e.target_equip")) %>%
        pivot_longer(cols = -e.id, names_to = "target", values_to = "rel") %>%
        filter(rel != "NULL") %>%
        mutate(target = as.integer(gsub("e.target_equip\\.", "", target))) %>%
        left_join(id_name_map, by = c("target" = "e.id")) %>%
        group_by(e.id) %>%
        summarise(target = paste(e.name, collapse = ", "), .groups = "drop")

      #Merge with equip_data
      equip_data <- equip_data %>%
        select(-starts_with("e.source"), -starts_with("e.target")) %>%
        left_join(source_equip, by = "e.id") %>%
        left_join(target_equip, by = "e.id")

  }
  
    # Join points and equipment metadata
    metadata <- full_join(equip_data, points_data, by = c("e.id" = "p.equip_id")) %>%
      mutate(
        p.tagged_units = ifelse(is.na(p.tagged_units), p.units, as.character(p.tagged_units))) %>% 
      #Rename some fields
      rename(
        e.equipment_id = e.id,
        p.point_id = p.id,
        p.point_type = p.type,
        e.equip_type = e.equip_type_tag,
        building_id = e.building_id) %>% 
      mutate(across(c(p.first_updated, p.last_updated), 
                    ~ convert_to_datetime(.))) %>%
      select(where(~ !all(is.na(.))))
    
    # Drop irrelevant columns
    drop_cols <- paste(c(
      "p.building_id", "_type_name", "_type_abbr", "_subtype",
      "flow", "\\.y", "child", "parent_equip", "measurement",
      "raw_unit_id", "hash", "e.points", "e.tags"
    ), collapse = "|")
      
    metadata <- metadata %>%
      select(-matches(drop_cols)) %>%
      select(sort(tidyselect::peek_vars()))

    
    
    if (verbose) cat("Metadata generated.\n")
    return(metadata)
}  
