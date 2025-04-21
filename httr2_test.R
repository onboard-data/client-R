api.setup()

pt <- get_point_types()

users <- get_users()

dep <- get_deployments()
dep <- api.request("deployment")

bldgs <- search_buildings(buildings = c("Gilmore","Business","Denny"))

metadata <- get_metadata(buildings = bldgs$id)

staged_data <- get_staging_data(buildings = 667)

#Create Equipment
api.request(endpoint = "staging/667/equipment/xoxo1",method = "POST")

#Delete equip
api.request(endpoint = "staging/667/equipment/xoxo1",method = "DELETE")

update_staging_equip(building = 667,
                     staging_equip = data.frame(name="xoxo",equipment_type_tag_name="HVAC/AHU"),
                     proceed = TRUE)


update_staging_points(building = 667,
                      staging_points = data.frame(equip_names = "xoxo",topic="onboard/HQ/bacnet-3114202/binary-value_19"),proceed = TRUE)

promote(building = 667,equipment = "a6dc8d16-8aa0-4cab-a94f-2c0d3fb24f5d",proceed = TRUE)

demote(building = 667,equipment_ids = 59035,proceed = TRUE)



query <- PointSelector()
query$buildings <- "Gilmore"
query$point_types <- c("cooling_thermal_energy_accumulator")
selection <- select_points(query,verbose = FALSE)

points <- get_points_by_ids(point_ids = selection$points,verbose = FALSE)

equip <- get_equipment_by_ids(equipment_ids = selection$equipment,verbose = FALSE)

metadata <- get_metadata(selection = selection)

start_time = "2025-04-01 00:00:00"
end_time = "2025-04-01 23:00:00"
point_ids = selection$points

ts <- get_timeseries_raw(start_time,end_time,point_ids)
ts <- get_timeseries(start_time,end_time,point_ids)


api.setup("dev")

api.request(endpoint = "staging/876/equipment/ahu/1X",method = "POST")
api.request(endpoint = "building/876/location/Room%2F1X?type=ROOM",method = "POST")
