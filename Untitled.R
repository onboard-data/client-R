buildings <- "art|bbh|business|cedar|chandlee|earle|food|forest reso|gradua|katz|Mill|Muell|Paterno|Pattee|whitmore"

staged_data <- get_staging_data(buildings = buildings)

#Define Required Columns
required_cols <- c('building_id','building_name','e.promoted_equipment_id',
                   'p.promoted_point_id','e.name','p.equip_ids',
                   'e.data.equip_type','e.data.objectName',
                   'e.data.area_served_desc','e.data.equip_dis',
                   'd.device_id','d.created','d.properties.address','d.name',
                   'd.properties.vendorName','d.properties.modelName',
                   'p.device_id','p.data.address','p.properties.description',
                   'p.properties.objectName','p.data.point_type','p.objectType',
                   'p.data.objectIdentifier',
                   'p.properties.presentValue','p.data.stateText',
                   'p.tagged_units',
                   'p.data.tagged_units','p.properties.units','p.topic',
                   'p.properties.last_discovery','p.created',
                   'p.last_promoted','p.last_updated')

#Combine staged data and metadata and filter stale points
staged_data <- staged_data[, intersect(required_cols,colnames(staged_data)),drop=FALSE]  %>% 
  mutate(across(c(p.last_updated,p.last_promoted), ~gsub('\\..*','',.)))  %>% 
  mutate(across(c(p.last_updated,p.last_promoted), ~as.POSIXct(., format="%Y-%m-%dT%H:%M:%S", tz="UTC")))   

staged_filter <- staged_data %>% filter(
                        !grepl("mb|meter|elev", p.topic),
                        !is.na(p.promoted_point_id),
{is.na(e.promoted_equipment_id)|e.promoted_equipment_id==""},
#p.last_updated < "2025-02-25"
#is.na(p.device_id),
is.na(e.name)
)

metadaa <- get_metadata(buildings = "business")
