# Building Metadata 

# Search Buildings --------------------------------------------------------

#' Search Buildings
#' 
#' Search any buildings in your org
#' 
#' @param buildings Integer, character, or vectors of those types, providing building id(s) or name(s). You can provide multiple buildings at once.
#' 
#' @param verbose Logical. If TRUE (default), print status messages.
#' 
#' @returns A dataframe with the building info results 
#' 
#' @export
search_buildings <- function(buildings = NULL, verbose = TRUE){
  
all_buildings <- api.get("buildings")
  
  if(!any(c("id","name") %in% names(all_buildings))){
    stop("id/name missing from data.")
  }
  
     search_text <- paste(buildings,collapse = "|")

     result <- all_buildings %>% filter(id %in% buildings |
                                 grepl(search_text, name, ignore.case = TRUE))
     
     if(verbose){
       cat(sprintf('Found building(s): %s',paste(result$name, collapse = ", ")))
     }
  return(result)
}

# Metadata ----------------------------------------------------------------


#' Metadata
#' 
#' Retrieves points and equipment for a given building or selection and outputs a clean metadata data.frame.
#' 
#' @inheritParams search_buildings
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
get_metadata <- function(buildings, selection = NULL, verbose = TRUE){
  
  if(missing(selection) & missing(buildings)){
    stop('Provide either building name/id or selection list.')
  } else if (missing(selection)){
    
    building_info <- search_buildings(buildings = buildings)
    
    building_ids <- building_info$id
    
    building_names <- building_info$name
    
    equip_data <- data.frame()
    points_data <-data.frame()
    
    for (i in 1:length(building_ids)){
    
    bid <- building_ids[i]  
    bname <- building_names[i]
      
    if(verbose){
        cat(sprintf("Querying equipment & points for building: %s (bid:%s)...\n",bname,bid))
      }  
    equip_data_bid <- api.get(paste0("buildings/",bid,"/equipment"))
    equip_data <- plyr::rbind.fill(equip_data, equip_data_bid)
    
    points_data_bid <- api.get(paste0("buildings/",bid,"/points"))
    points_data <- plyr::rbind.fill(points_data, points_data_bid)
    
    }
  } else if (missing(buildings)){
  
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
  points_data <- get_points_by_ids(point_ids)
  
  if(verbose){
    cat(sprintf('Querying %s equipment...\n',length(equipment_ids)))
  }
  equip_data <- get_equipment_by_ids(equipment_ids)
  }
  
  points_data <- points_data %>%
    mutate(across(equip_id, ~gsub("c\\(|\\)", "", .))) %>% # Remove "c()" if present
    separate_rows(equip_id, sep = ",\\s*") %>%      # Split by comma and optional space
    mutate(across(equip_id,~ as.numeric(.)))    
  
  points_data_names <- names(points_data)
  points_data_names <- paste0('p.', points_data_names)
  names(points_data) <- points_data_names
  
  equip_data_names <- names(equip_data)
  equip_data_names <- paste0('e.', equip_data_names)
  names(equip_data) <- equip_data_names
  
  #Create a metadata for the specified building ID
  metadata <- equip_data %>% 
    # Grab Equip Refs by joining with Equip DB again
    # mutate(e.parent_equip = as.integer(.data$e.parent_equip)) %>% 
    # left_join(
    #   select(equip_data, .data$e.id, .data$e.equip_id),
    #   by = c('e.parent_equip' = 'e.id'),
    #   suffix = c('', '.y')
    # ) %>% 
    # rename(e.parent = .data$e.equip_id.y) %>%
    full_join(points_data,
                         by = c('e.id' = 'p.equip_id')) %>% 
    #Remove NA equipment rows
    filter(!is.na(e.equip_id)) %>% 
    #Get tagged units if NA
    mutate(across(.data$p.tagged_units,
                  ~ifelse(is.na(.),
                          p.units,as.character(.))))  %>% 
    #Rename some fields
    rename(e.equipment_id = .data$e.id,
           p.point_id = .data$p.id,
           p.point_type = .data$p.type,
           e.equip_type = .data$e.equip_type_tag,
           building_id=e.building_id) %>%  
    #Convert epoch time-stamps to UTC 
    mutate(across(c(.data$p.first_updated, .data$p.last_updated),
                  ~ as.POSIXct(as.integer(substr(.,1,10)),
                               origin = '1970-01-01',
                               tz = 'UTC')))  %>% 
    select(where(~!all(is.na(.))))
  
  #Columns to remove from metadata
  rem_col <- paste('p\\.building_id','_type_name','_type_abbr','_subtype',
                   'flow','\\.y','child','parent_equip','measurement',
                   'raw_unit_id','hash','e\\.points','e\\.tags', sep ="|")
  
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
