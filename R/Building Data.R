
# Get Building Data -------------------------------------------------------

#Takes building id or name. Gives a text output of the building id and name
get_building_info <- function(buildings){

  query <- PointSelector()

  query$buildings <- buildings

  selection <- select_points(query)

  buildings_id <- unlist(selection$buildings)

  building_db <- api.get('buildings')

  building_db_filter <- building_db[building_db$id %in% buildings_id,]

  id <- building_db_filter$id
  name <- building_db_filter$name

  assign('id',id,envir=parent.frame())
  assign('name',name,envir = parent.frame())

  print(sprintf('Building ID: %s, Building Name: %s',id,name))

}

##Get clean metadata by building names or ids
get_metadata <- function(buildings){

  get_building_info(buildings)

  query <- PointSelector()

  query$buildings <- buildings

  selection <- select_points(query)

  print('Querying Points...')
  points <- get_points_by_ids(selection$points)

  print('Querying Equipment...')
  equipment <- get_equipment_by_ids(selection$equipment)

  #Create a metadata for the specified building ID
  metadata <- inner_join(equipment,points,
                         by = c('id' = 'equip_id'),
                         suffix = c('', '.y')) %>%
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
    # remove NAs
    mutate_all( ~ replace_na(., '')) %>%
    #Convert unix time-stamps to EST
    mutate_at(vars(first_updated, last_updated),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York'))

  print('Metadata generated.')
  return(metadata)
}



