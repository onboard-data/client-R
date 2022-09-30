
# GET ---------------------------------------------------------------------


#' API GET call
#'
#' @description
#'
#' Uses http GET call to return an object from the API.
#'
#' @param endpoint A character string containing a valid Onboard API endpoint.
#' 
#' @return An R object of class `list` or `data.frame`
#' 
#' @examples
#' \dontrun{ whoami <- api.get('whoami') }
#' 
#' @export
api.get <- function(endpoint) {
  
  api.access()
  
  # get endpoint
  endpoint_url <- paste(api_url, endpoint, sep = '/')
  
  request_endpoint <- GET(url = endpoint_url,
                          content_type_json(),
                          add_headers(`X-OB-Api` = api_key))
  
  if (request_endpoint$status_code == 200) {
    
      api_output <-
        content(request_endpoint, as = 'text', encoding = 'UTF-8') %>% 
        fromJSON(flatten = T)
    return(api_output)
    
  } else{
    stop(paste0('API Status Code: ', request_endpoint$status_code))
    
  }
}


# POST --------------------------------------------------------------------

#' API POST call
#' 
#' @description 
#' 
#' Uses http POST call to post objects to the API.
#' @inheritParams  api.get
#' 
#' @param json_body A JSON payload to give to the POST call.
#' 
#' @param output If "list" (default), it returns the api output as a list object. If "dataframe", it returns the api output as a dataframe object
#' 
#' @return An R object of `list` or `data.frame` class
#' 
#' @export
api.post <- function(endpoint, json_body, output = 'list') {
  
  api.access()
  
  # post endpoint
  endpoint_url <- paste(api_url, endpoint, sep = '/')
  
  request_endpoint <- POST(
    url = endpoint_url,
    content_type_json(),
    add_headers(`X-OB-Api` = api_key),
    body = json_body
  )
  
  if (request_endpoint$status_code == 200) {
    if (output == 'list') {
      api_output <- content(request_endpoint)
      
    } else if (output == 'dataframe') {
      api_output <-
        content(request_endpoint, as = 'text',
                encoding = 'UTF-8') %>%
        fromJSON(flatten = T)
      
      if (inherits(api_output, 'list')) {
        stop("Cannot convert output to dataframe. Please use output = 'list'")
        
      }
      api_output <- as.data.frame(api_output)
      
    } else {
      stop("'output' must be 'list' or 'dataframe'")
      
    }
    return(api_output)
  } else {
    stop(paste0('API Status Code: ', request_endpoint$status_code))
    
  }
  
}


# DELETE ------------------------------------------------------------------

#' API DELETE CALL
#' 
#' Uses http DELETE call to post objects to the API. Do not use this unless required. It is recommended to use the staging area functions to skip equipment or points from the main building. 
#' 
#' @param building Building id or name. Note: If you enter multiple building ids or names, only the first entry is considered
#' 
#' @param entity 'points' or 'equipment'
#' 
#' @param db_id Provide database ids belonging to either equipment or points.
#' 
api.delete <- function(building, entity, db_id){
    
  get_building_info(building)
  
  api.access()
  
  endpoint <- paste('buildings',id,entity,sep='/')
  
  for (i in 1:length(db_id)) {
    
    single_id <- db_id[i]
    
    endpoint_url <- paste(api_url, endpoint, single_id, sep = '/')
    
    execute_object <- DELETE(
      url = endpoint_url,
      content_type_json(),
      add_headers(`X-OB-Api` = api_key)
    )
    
    if (execute_object$status_code == 200) {
      print(sprintf('Deleted %s of %s',i,
                    length(db_id)))
    }
    else {
      print(paste0('Status Code is ', execute_object$status_code))
      break
    }
  }
}

