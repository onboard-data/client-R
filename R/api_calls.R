# GET ---------------------------------------------------------------------
#' API GET call
#'
#' @description
#'
#' Uses http GET call to return an object from the API.
#'
#' @param endpoint A character string containing a valid Onboard API endpoint.
#' 
#' @return A list or data.frame of the API output.
#' 
#' @examples
#' \dontrun{ whoami <- api.get('whoami') }
#' 
#' @export
api.get <- function(endpoint) {
  api_data <- api.access()
  
  # get endpoint
  endpoint_url <- paste(api_data$url, endpoint, sep = '/')
  
  request_endpoint <- GET(url = endpoint_url,
                          content_type_json(),
                          add_headers(`X-OB-Api` = api_data$key))
  
  if (request_endpoint$status_code == 200) {
      api_output <-
        content(request_endpoint, as = 'text', encoding = 'UTF-8') %>% 
        fromJSON(flatten = TRUE)
    return(api_output)
  } else {
    stop(httr::http_status(request_endpoint$status_code)$message)
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
#' @param upload_path (Optional) A character string containing the file path for the file to upload
#' 
#' @param output A character string, either "list" (default) or "dataframe", to specify the API output format.
#' 
#' 
#' @return A list or data.frame of the API output.
#' 
#' @export
api.post <- function(endpoint, json_body, upload_path = NULL, output = 'list') {
  api_data <- api.access()
  
  # post endpoint
  endpoint_url <- paste(api_data$url, endpoint, sep = '/')
  
  if(length(upload_path) == 0){
  request_endpoint <- POST(
    url = endpoint_url,
    content_type_json(),
    add_headers(`X-OB-Api` = api_data$key),
    body = json_body
  )
  } else {
    request_endpoint <- POST(
      url = endpoint_url,
      add_headers(`X-OB-Api` = api_data$key),
      body = list(file = upload_file(upload_path)),
      encode= 'multipart'
    )
  }
  
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
    stop(httr::http_status(request_endpoint$status_code)$message)
  }
  
}



# DELETE ------------------------------------------------------------------

api.delete <- function(endpoint, json_body = NULL){  
  
api_data <- api.access()

# get endpoint
endpoint_url <- paste(api_data$url, endpoint, sep = '/')

if(is.null(json_body)){
  request_endpoint <- DELETE(url = endpoint_url,
                             content_type_json(),
                             add_headers(`X-OB-Api` = api_data$key))
} else {  
request_endpoint <- DELETE(url = endpoint_url,
                        content_type_json(),
                        add_headers(`X-OB-Api` = api_data$key),
                        body = json_body)
}

if (request_endpoint$status_code == 200) {
  api_output <-
    content(request_endpoint, as = 'text', encoding = 'UTF-8') %>% 
    fromJSON(flatten = TRUE)
  return(api_output)
} else {
  stop(httr::http_status(request_endpoint$status_code)$message)
}
}
