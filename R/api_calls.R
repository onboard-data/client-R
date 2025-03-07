# GET ---------------------------------------------------------------------
#' API GET call
#'
#' @description Uses http GET call to return an object from the API.
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
  # Access API credentials from the local environment.
  api_data <- api.access()
  
  # Construct the endpoint URL
  endpoint_url <- file.path(api_data$url, endpoint)
  
  headers = api_data$headers
  
  # Make GET request with headers
  response <- GET(url = endpoint_url,headers)
  
  # Check for errors
  if (http_status(response)$category != "Success") {
    stop(paste("API call failed:", http_status(response)$message))
  }
  

  # Parse the response and flatten
  api_output <- content(response, as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
  return(api_output)
}



# PATCH -------------------------------------------------------------------

#' API PATCH call
#' 
#' @description Uses http PATCH call to post objects to the API.
#' 
#' @inheritParams  api.get
#' 
#' @param json_body A JSON payload to give to the POST call.
#' 
#' @return A list or data.frame of the API output.
#' 
#' @export
api.patch <- function(endpoint,json_body=NULL){
  
  # Access API credentials
  api_data <- api.access()
  
  # Construct the endpoint URL
  endpoint_url <- file.path(api_data$url, endpoint)
  headers = api_data$headers
  
  #PATCH Request
  response <- PATCH(
    url = endpoint_url,
    headers,
    body = json_body,
    encode = "json"
  )
  
  # Check for errors
  if (http_status(response)$category != "Success") {
    stop(paste("API call failed:", http_status(response)$message))
  }
  
  # Parse the response and flatten
  api_output <- content(response, as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
  return(api_output)
}


# POST --------------------------------------------------------------------

#' API POST call
#' 
#' @description Uses http POST call to post objects to the API.
#' 
#' @inheritParams  api.patch
#'
#' @param upload_path (Optional) A character string containing the file path for the file to upload
#' 
#' @param output A character string, either "list" (default) or "dataframe", to specify the API output format.
#' 
#' @return A list or data.frame of the API output.
#' 
#' @export
api.post <- function(endpoint, json_body = NULL, upload_path = NULL, output = 'list') {
  # Access API credentials
  api_data <- api.access()
  
  # Construct the endpoint URL
  endpoint_url <- file.path(api_data$url, endpoint)
  headers = api_data$headers
  
  # Create the POST request
  if (is.null(upload_path)) {
    # Regular POST with JSON body
    response <- POST(
      url = endpoint_url,
      headers,
      body = json_body,
      encode = "json"
    )
  } else {
    # POST with file upload
    response <- POST(
      url = endpoint_url,
      headers,
      body = list(file = upload_file(upload_path)),
      encode = "multipart",
      httr::add_headers(`Content-Type` = "multipart/form-data")
    )
  }
  
  # Check for errors
  if (http_status(response)$category != "Success") {
    stop(paste("API call failed:", http_status(response)$message))
  }
  
  
  # Parse the response based on the requested output format
  api_output <- switch(
    output,
    list = content(response),
    dataframe = {
      response_text <- content(response, as = "text", encoding = "UTF-8")
      parsed_json <- fromJSON(response_text, flatten = TRUE)
      
      if (!is.list(parsed_json)) {
        stop("Cannot convert output to dataframe. Please use output = 'list'")
      }
      as.data.frame(parsed_json)
    },
    stop("'output' must be 'list' or 'dataframe'")
  )
  
  return(api_output)
}

# DELETE ------------------------------------------------------------------

#' API DELETE call
#' 
#' @description Uses http DELETE call to post objects to the API.
#' 
#' @inheritParams  api.patch
#' 
#' @export
api.delete <- function(endpoint, json_body = NULL){  
  
  # Access API credentials
  api_data <- api.access() 

  # Construct the endpoint URL
  endpoint_url <- file.path(api_data$url, endpoint)
  
  headers = api_data$headers

  # DELETE request
  response <- DELETE(
    url = endpoint_url,
    headers,
    body = json_body,
    encode = if (!is.null(json_body))
      "json"
    else
      NULL
  )
  
  # Check for errors
  if (http_status(response)$category != "Success") {
    stop(paste("API call failed:", http_status(response)$message))
  }

  api_output <-
    content(response, as = 'text', encoding = 'UTF-8') %>% 
    fromJSON(flatten = TRUE)

    return(api_output)

}
