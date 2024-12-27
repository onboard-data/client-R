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
  # Access API credentials from the local environment.
  api_data <- api.access()  
  
  # Construct the endpoint URL
  endpoint_url <- file.path(api_data$url, endpoint)
  
  # Make GET request with headers
  response <- GET(url = endpoint_url,
                  add_headers(
                    `Content-Type` = "application/json", 
                    `X-OB-Api` = api_data$key))
  
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
  # Access API credentials
  api_data <- api.access()
  
  # Construct the endpoint URL
  endpoint_url <- file.path(api_data$url, endpoint)
  
  # Create the POST request
  if (is.null(upload_path)) {
    # Regular POST with JSON body
    response <- POST(
      url = endpoint_url,
      add_headers(`Content-Type` = "application/json", `X-OB-Api` = api_data$key),
      body = json_body,
      encode = "json"
    )
  } else {
    # POST with file upload
    response <- POST(
      url = endpoint_url,
      add_headers(`X-OB-Api` = api_data$key),
      body = list(file = upload_file(upload_path)),
      encode = "multipart"
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

api.delete <- function(endpoint, json_body = NULL){  
  
  # Access API credentials
  api_data <- api.access() 

  # Construct the endpoint URL
  endpoint_url <- file.path(api_data$url, endpoint)

  # Make the DELETE request
  response <- DELETE(
    url = endpoint_url,
    add_headers(`Content-Type` = "application/json", `X-OB-Api` = api_data$key),
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

  # Parse and return the response
  api_output <- content(response, as = "parsed", type = "application/json")  

    return(api_output)

}
