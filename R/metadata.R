# Building Metadata 


# Building Info -----------------------------------------------------------

#' Building Info
#' 
#' Retrieves building id(s) and name(s). Assigns each to list variables called "id" and "name", and prints each list.
#' 
#' @param buildings A character vector or integer. Provide either building id or name. You can provide multiple buildings at once.
#'
#' @examples 
#' \dontrun{
#' get_building_info(buildings=c(427,"Laboratory"))
#' }
#'
get_building_info <- function(buildings){
  
  all_buildings <- get_buildings()
  
  building_info <- data.frame()
  
  for(i in 1:length(buildings)){
    
    single <- buildings[i]
    
    building <- all_buildings[all_buildings$name==single,]
    
    if(nrow(building)==0) {
      
      building <- all_buildings[all_buildings$id == single, ]
      
      if (nrow(building) == 0) {
        
        stop(sprintf('No building found for %s.',single))
      }
    }
    
    building <- select(building,id,name)
    
    building_info <- rbind(building_info,building)
    
  }
  
  id <- building_info$id
  name <- building_info$name
  
  assign('id',id,envir=parent.frame())
  assign('name',name,envir = parent.frame())
  
  print(sprintf('Building ID: %s, Building Name: %s',id,name))
  
}

# Metadata ----------------------------------------------------------------


#' Metadata
#' 
#' Retrieves points and equipment for a given building or selection and outputs a clean metadata dataframe.
#' 
#' @inheritParams get_building_info
#' @param selection Selection list from point selector.
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
get_metadata <- function(buildings,selection){
  
  if(missing(selection) & missing(buildings)){
    stop('Provide either building name/id or selection list.')
  } else if (missing(selection)){
    
    get_building_info(buildings)
    
    query <- PointSelector()
    
    query$buildings <- buildings
    
    selection <- select_points(query)
  }
  
  if(length(selection$points)==0){
    stop('No metadata found.')
  }
  
  point_ids <- selection$points
  equipment_ids <- selection$equipment
  
  print(sprintf('Querying %s Points...',length(point_ids)))
  points <- get_points_by_ids(point_ids)
  
  print(sprintf('Querying %s Equipment...',length(equipment_ids)))
  equipment <- get_equipment_by_ids(equipment_ids)
  
  #Create a metadata for the specified building ID
  metadata <- inner_join(equipment,points,
                         by = c('id' = 'equip_id'),
                         suffix = c('', '.y')) %>%
    #Get tagged units if NA
    mutate(across(tagged_units,
                  ~ifelse(is.na(.),
                          units,as.character(.)))) %>%
    #Replace equip_type tag with subtype tag if present
    mutate(across(equip_type_tag,
                  ~ifelse(is.na(equip_subtype_tag),
                          .,equip_subtype_tag))) %>%
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
    #Convert unix time-stamps to EST
    mutate(across(c(first_updated, last_updated),
                  ~ as_datetime(as.numeric(substr(., 1, 10)))))
  
  print('Metadata generated.')
  return(metadata)
}