#datamodel

# Equipment Types --------------------------------------------------------------

#' Equipment Types
#' 
#' Query all equipment types from Onboard's Data Model.
#' 
#' @returns equip_type: A dataframe object.
#' @export
get_equip_types <- function(){
  
  equiptype <- api.get('equiptype',output = 'datframe')
  
  subtypes <- sapply(equiptype$sub_types,as.data.frame)
  subtypes <- data.table::rbindlist(subtypes)
  
  equip_types <- equiptype %>%
    filter(active==T) %>%
    select(-c(sub_types,critical_point_types,flow_order,active)) %>%
    left_join(subtypes,by=c('id'='equipment_type_id'),
              suffix = c('','_subtype'))
  
  return(equip_types)
}

# Point Types -------------------------------------------------------------

#' Point Types
#' 
#' Queries all point types, measurements and their units from Onboard's Data Model and returns a clean output.
#' 
#' @return point_types: A dataframe object.
#' @export
get_point_types <- function(){
  
  pointtypes <- api.get('pointtypes',output='dataframe')
  
  measurements <- api.get('measurements',output='dataframe')
  
  # Get measurements and their associated units
  units <- data.frame()
  
  for (i in 1:nrow(measurements)){
    row_num =i
    
    measurement_single <- measurements[row_num,]
    
    measurement_units <- measurement_single$units
    measurement_units <- data.table::rbindlist(measurement_units)
    measurement_units[,'measurement_id']<- measurement_single$id
    
    units <- plyr::rbind.fill(units,measurement_units)
  }
  
  units <- units %>%
    select(measurement_id,
           unit_name=name_long,
           unit=name_abbr,data_type) %>%
    mutate(measurement_id=as.integer(measurement_id))
  
  measurements_units <- left_join(measurements,
                                  units,
                                  by=c('id' = 'measurement_id')) %>%
    select(id,
           measurement_name=name,
           units_convertible,
           qudt_type,
           unit_name,
           unit,
           data_type)
  
  #Unite data frames
  point_types <- left_join(select(
    pointtypes,id,tag_name,measurement_id,tags),
    measurements_units,
    by = c("measurement_id" = 'id')) %>%
    mutate(across(tags,  ~ gsub('c\\(|\\)', '', .))) %>%
    select(id,
           point_type = tag_name,
           measurement_name,
           unit,
           data_type,
           tags)
  
  return(point_types)
  
}

