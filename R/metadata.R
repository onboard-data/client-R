# Building Metadata 


# Building Info -----------------------------------------------------------

#' Building Info
#' 
#' Returns building id(s) and name(s) in a named list.
#' 
#' @param buildings Integer, character, or vectors of those types, providing building id(s) or name(s). You can provide multiple buildings at once.
#' 
#' @param verbose Logical. If TRUE (default), print status messages.
#'
#' @return A data.frame of building info with two columns, 'id' and 'name'.
#' 
#' @export
get_building_info <- function(buildings, verbose = TRUE){
  
  all_buildings <- get_buildings()
  
  building_info <- data.frame()
  
  for(i in 1:length(buildings)){
    
    single <- buildings[i]
    
    building <- all_buildings[all_buildings$name == single,]
    
    if(nrow(building) == 0) {
      
      building <- all_buildings[all_buildings$id == single, ]
      
      if (nrow(building) == 0) {
        stop(sprintf('No building found for name/id: %s\n',single))
      }
    }
    
    building <- select(building, .data$id, .data$name)
    
    building_info <- rbind(building_info, building)
    
  }
  
  if(verbose){
    cat(sprintf('\nFound building(s): %s...\n',
                  paste(building_info$name,collapse=', ')))
  }

  
  return(building_info)
}

# Metadata ----------------------------------------------------------------


#' Metadata
#' 
#' Retrieves points and equipment for a given building or selection and outputs a clean metadata data.frame.
#' 
#' @inheritParams get_building_info
#' 
#' @param selection Selection list from point selector.
#' 
#' @return A data.frame of clean metadata for the requested points.
#' 
#' @examples 
#' \dontrun{
#' metadata <- get_metadata(buildings=c(427,"Laboratory"))
#' 
#' OR
#' 
#' query <- PointSelector()
#' 
#' query$buildings <- 427
#' query$equipment_types <- 'ahu'
#' query$point_types <- c('Supply Air Temperature','Supply Air Static Pressure')
#' 
#' selection <- select_points(query)
#' 
#' metadata <- get_metadata(selection)
#' }
#' 
#' @export
get_metadata <- function(buildings = NULL, selection = NULL, verbose = TRUE){
  
  if(missing(selection) & missing(buildings)){
    stop('Provide either building name/id or selection list.')
  } else if (missing(selection)){
    
    building_info <- get_building_info(buildings, verbose = verbose)
  
    query <- PointSelector()
    
    query$buildings <- building_info$id
    
    selection <- select_points(query)
  }
  
  if(!is.list(selection) | is.atomic(selection)){
    stop('selection should be a non-atomic named list with some subset of fields: 
         c(orgs, buildings, point_ids, point_names, point_topics, updated_since, 
         point_types, equipment, equipment_types, point_hashes)')
  }
  
  if(length(selection$points)==0){
    stop('No metadata found.')
  }
  
  point_ids <- selection$points
  equipment_ids <- selection$equipment
  
  if(verbose){
    cat(sprintf('Querying %s points...\n',length(point_ids)))
  }
  
  ## Get points
  points_data <- get_points_by_ids(point_ids) %>% 
  mutate(across(.data$equip_id, ~as.integer(.)))

  points_data_names <- names(points_data)
  points_data_names <- paste0('p.', points_data_names)
  names(points_data) <- points_data_names
  
  if(verbose){
    cat(sprintf('Querying %s equipment...\n',length(equipment_ids)))
  }

  #Get equipment
  equip_data <- get_equipment_by_ids(equipment_ids)
  
  equip_data_names <- names(equip_data)
  equip_data_names <- paste0('e.', equip_data_names)
  names(equip_data) <- equip_data_names
  
  #Create a metadata for the specified building ID
  metadata <- inner_join(equip_data,points_data,
                         by = c('e.id' = 'p.equip_id')) %>% 
    #Get tagged units if NA
    mutate(across(.data$p.tagged_units,
                  ~ifelse(is.na(.),
                          units,as.character(.)))) %>% 
    #Rename some fields
    rename(e.equipment_id = .data$e.id,
           p.point_id = .data$p.id,
           p.point_type = .data$p.type,
           e.equip_type = .data$e.equip_type_tag,
           building_id=e.building_id) %>% 
    # Grab Equip Refs by joining with Equip DB again
    mutate(e.parent_equip = as.integer(.data$e.parent_equip)) %>% 
    left_join(
      select(equip_data, .data$e.id, .data$e.equip_id),
      by = c('e.parent_equip' = 'e.id'),
      suffix = c('', '.y')
    ) %>% 
    rename(e.parent = .data$e.equip_id.y) %>% 
    #Convert epoch time-stamps to UTC 
    mutate(across(c(.data$p.first_updated, .data$p.last_updated),
                  ~ as.POSIXct(as.integer(substr(.,1,10)),
                               origin = '1970-01-01',
                               tz = 'UTC'))) 
  
  #Columns to remove from metadata
  rem_col <- paste('p.building_id','type_id','type_name','type_abbr','subtype',
                   'flow','.y','child','parent','measurement',
                   'raw_unit','hash','points','tags', sep ="|")
  
  metadata_cols <- data.frame(names=colnames(metadata)) %>% 
  filter(!grepl(rem_col,names)) 
  
  metadata <- metadata %>% 
    select(metadata_cols$names) %>% 
    select(sort(tidyselect::peek_vars())) 
  
  #Handle state_texts
  if(grepl("state_text",metadata_cols)){
    
    if(verbose){
      cat("Handling state_text fields...\n")
    }
    
    state_text_fields <- metadata_cols %>% 
      filter(grepl("state_text",names)) %>% 
      pull()
      
    metadata <- metadata %>% 
      tidyr::unite('p.state_text',all_of(state_text_fields),sep=", ",na.rm = TRUE)
  }
  
  if(verbose){
    cat('Metadata generated.\n')
  }

  return(metadata)
}