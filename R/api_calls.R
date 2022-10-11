
#' API Error Handler
#' @description 
#' Handles http errors, returning a useful formatted message.
#' 
#' @param status_code An integer, should be from 400-599, in this case the http error code returned by a faulty server request.
#' 
#' @return A string with nicely-formatted error description.
api_error <- function(status_code){
  return(paste0('API Error: (', status_code, ') ', httr::http_status(status_code)$reason))
}


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
    stop(api_error(request_endpoint$status_code))
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
    stop(api_error(request_endpoint$status_code))
  }
  
}