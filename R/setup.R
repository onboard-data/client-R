#' Check the status of API connection
#' 
#' @description 
#' Provides a status code and message for the API connection.
#' 
#' @return A character string of the API server status and message.
#'
#' @export
api.status <- function() {
  api_data <- api.access()
  
  request <- GET(url = api_data$url)
  
  return(httr::http_status(request$status_code)$message)
}

#' Set up Onboard API keys and URL in system environment
#' @description
#' 
#' Set the Onboard API URL and API keys in the system environment.
#'  
#' @param type Provide the API client name.
#' 
#' @param key Option to include your API keys in the argument. It is NULL by default
#' 
#' @param token Option to include your Auth token in the argument. It is NULL by default
#' 
#' @param verbose Logical. If TRUE, prints the API status.
#' 
#' @return No return value, sets API URL and API key in the system environment.
#'   
#' @export
#' 
api.setup <- function(type = 'prod', key = NULL, token = NULL,verbose = TRUE) {
  
  Sys.setenv("url" = "")
  Sys.setenv("key" = "")
  Sys.setenv('token' = "")
  
  
  if(!(type %in% c('prod','dev','rtem','gcp'))){
    stop("Please use 'prod', 'dev', 'gcp' or 'rtem' for api_type")
  }
  
  url <- dplyr::case_when(
    type == 'prod' ~ 'https://api.onboarddata.io',
    type == 'dev' ~ 'https:/devapi.onboarddata.io',
    type == 'rtem' ~ 'https://api.ny-rtem.com',
    type == 'gcp' ~ 'https://rews.onboarddata.io/api'
  )
  
  Sys.setenv('url' = url)
  
  if (!is.null(token)) {
    Sys.setenv('token' = token)
  } else {
    if (is.null(key)) {
      if (Sys.getenv("RSTUDIO") == "1") {
        api_name <- paste0('api_key_', type)
        
        key <- rstudioapi::askForSecret(name = api_name,
                                            message = 'Enter your API key here',
                                            title = "Onboard API Key")
      } else {
        key <- readline(prompt = "Enter your Onboard API key:")
      }
      Sys.setenv('key' = key)
    }
  }
    
   if(verbose){
     cat(api.status())
   }
}

#' Access API keys and URL from System Environment
#' @description 
#' 
#' Returns the API url and API key.
#' 
#' @return A named list of API information, containing elements 'url' and 'key'.
#' 
api.access <- function(){
  url <- Sys.getenv('url')
  key <- Sys.getenv('key')
  token <- Sys.getenv('token')
  
  if(url == '' | (key == '' & token== '')){
    stop('API credentials not set correctly.')
  } else {
    return(list(
      'url' = url,
      'key' = key,
      'token' = token
    ))
  }
}