#' Set up Onboard API keys and URL in system environment
#' @description
#' 
#' Set the Onboard API URL and API keys in the system environment.
#'  
#' @param api_type Provide the API client name
#' 
#'   If `prod` (default), sets API URL and keys for accessing Onboard's PROD API.
#'   
#'   If `dev`: sets API URL and keys for accessing Onboard's DEV API.
#'   
#'   If `rtem`: sets API URL and keys for accessing Onboard's RTEM API
#' 
#' @export
#' 
api.setup <- function(api_type = 'prod') {
  
  if(api_type == 'prod') {
    api_url <- 'https://api.onboarddata.io'
    
    api_key <- rstudioapi::askForSecret(
      name='api_key_prod',
      message = 'Enter your API keys here',
      title="Onboard API Keys")
    
  } else if (api_type == 'dev') {
    api_url <- 'https://devapi.onboarddata.io'
    
    api_key <- rstudioapi::askForSecret(
      name = 'api_key_dev',
      message='Enter your DEV API keys here',
      title = "Onboard API Keys")
  } else if(api_type == 'rtem') {
    api_url <- 'https://api.ny-rtem.com'
    
    api_key <- rstudioapi::askForSecret(
      name = 'api_key_rtem',
      message = 'Enter your RTEM API keys here',
      title = 'Onboard API Keys'
    )
  } else {
    stop("Please use 'prod' or 'dev' for api_type")
  }
  
  Sys.setenv('api_url'=api_url)
  Sys.setenv('api_key'=api_key)
  
  
}



#' Access API keys and URL from System Environment
#' @description 
#' 
#' `api.access()` is used to manually access api_url and api_keys stored in the system environment.
api.access <- function(){
  
  api_url <- Sys.getenv('api_url')
  
  api_key <- Sys.getenv('api_key')
  
  if(api_url==''|api_key==''){
    stop('API credentials not set correctly.')
  } else {
    assign('api_url',api_url,parent.frame())
    assign('api_key',api_key,parent.frame())
  }
}

#' Check the status of your connection with the Onboard API
#' 
#' @description 
#' Gives a status code for api connection.
#'  
#' @export
api.status <- function() {
  
  api.access()
  
  request <- GET(url = api_url,
                 add_headers(`X-OB-Api` = api_key))
  
  print(paste0("API Status: ", request$status_code))
  
}
