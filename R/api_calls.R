
# GET ---------------------------------------------------------------------


#' API GET call
#'
#' @description
#'
#' Uses http GET call to return an obect from the API
#'
#' @param endpoint A character string containing a valid Onboard API endpoint
#'
#' @returns 
#'`api.get()` returns an R object of `list` or `data.frame` class
#'
#' @examples
#' whoami <- api.get('whoami')
#'
#'@export
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
#' Uses http POST call to post objects to the API
#' @inheritParams  api.get
#' 
#' @param json_body A JSON payload to give to the POST call
#' 
#' @param output if "dataframe" (default), it returns the api output as a dataframe object. If "list", it returns the api output as a list object
#' 
#'@export
api.post <- function(endpoint, json_body, output) {
  if (missing(output)) {
    output = 'dataframe'
  }
  
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
    if (output == 'dataframe') {
      api_output <-
        content(request_endpoint, as = 'text',
                encoding = 'UTF-8') %>%
        fromJSON(flatten = T)
    } else if (output == 'list') {
      api_output <- content(request_endpoint)
    }
    
    return(api_output)
    
  } else{
    stop(paste0('API Status Code: ', request_endpoint$status_code))
    
  }
  
}


