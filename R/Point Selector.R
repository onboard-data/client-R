#Point Selector Function
PointSelector <- function(){
 query <- list(orgs='',buildings='',point_ids='',point_names='',
              point_topics='',updated_since='',point_types='',
              equipment='',equipment_types='',point_hashes='')
  
  return(query)
}

#Point Selector function
select_points <- function(query){

  if(query$updated_since!='') {
    updated_since <- as.numeric(as.POSIXct(query$updated_since))
  } else {
    updated_since<- ''
  }
  
  query <- query[names(query)!='updated_since']
  
  query_json <- query %>% 
    toJSON()

  query_json <- gsub('\\[""\\]','[]',query_json) 
  
  if(updated_since!=''){
  query_json <- 
    gsub('}',paste0(',"updated_since":',updated_since,'}'),query_json) 
  }
  
endpoint_url <- paste(api_url,'points/select',sep='/')

  
request_endpoint <- POST(url=endpoint_url,
                         content_type_json(),
                         add_headers(`X-OB-Api` = api_key),
                         body=query_json)


if(request_endpoint$status_code==200){

point_selector_output <- content(request_endpoint)

return(point_selector_output)
} else {
  stop(sprintf('API Status Code: %s',request_endpoint$status_code))
}

}

#query points by id
get_points_by_ids <- function(id){
  
  id_unlist <- unlist(id)

  chunks <- split(id_unlist,
                        ceiling(seq_along(id_unlist)/500))
  
  points<-data.frame()
  
  for(i in 1:length(chunks)){
    
    point_ids <- toJSON(chunks[[i]])
    
    #URL encode JSON list of points
    point_ids <- URLencode(point_ids,reserved = T)
    
    endpoint <- paste0('points?point_ids=',point_ids)
    
    points_chunk <- api.get(endpoint)
  
    points <- rbind.fill(points,points_chunk)
}
  
return(points)
  
}

#query equipment by id
get_equipment_by_ids <- function(id){
  
  id_unlist <- unlist(id)
  
  #Convert list of ids to JSON payload
  id_json <- list(equipment_ids=id_unlist) %>% 
    toJSON()
  
  equipment <- api.post(endpoint='equipment/query',
                        json_body = id_json)
  
  return(equipment)
}
