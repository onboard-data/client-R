#Patch Point IDs

# Patch Point Type --------------------------------------------------------

# Udpate a point_type using PATCh request. 
# Make sure endpoint_db has building_id, point_id, and new_point_type_id

api.patch_point_type_id <- function(api_type,endpoint_db){
  
  api.keys(api_type)
  
  for (i in 1:nrow(endpoint_db)) {
    
    row_no <- i
    
    building_id <- endpoint_db$building_id[row_no]
    
    single_id <- endpoint_db$id[row_no]
    
    endpoint <- paste('buildings',building_id,'points',sep='/')
    
    endpoint_url <- paste(api_url, endpoint, single_id, sep = '/')
    
    patch_id <- endpoint_db$point_type_id[row_no]
    
    execute_object <- PATCH(
      url = endpoint_url,
      content_type_json(),
      add_headers(`X-OB-Api` = api_key),
      body=paste0('{"point_type_id": ',patch_id,'}')
    )
    
    if (execute_object$status_code == 200) {
      svMisc::progress(value = i, max.value = nrow(endpoint_db))
    }
    else {
      print(paste0('Status Code is ', execute_object$status_code))
      break
    }
  }
}