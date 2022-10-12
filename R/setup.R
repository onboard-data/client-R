#' Set up Onboard API keys and URL in system environment
#' @description
#' 
#' Set the Onboard API URL and API keys in the system environment.
#'  
#' @param api_type Provide the API client name
#'   "prod" (default): sets API URL and keys for accessing Onboard's API.
#'   
#' @export
#' 
api.setup <- function(api_type = 'prod') {
  if(!(api_type %in% c('prod','dev','rtem'))){
    stop("Please use 'prod', 'dev', or 'rtem' for api_type")
  }
  
  api_url <- dplyr::case_when(
    api_type == 'prod' ~ 'https://api.onboarddata.io',
    api_type == 'dev' ~ 'https://devapi.onboarddata.io',
    api_type == 'api_rtem' ~ 'https://api.ny-rtem.com'
  )
  
  api_name <- paste0('api_key', api_type)
  
  if (Sys.getenv("RSTUDIO") == "1"){
    api_key <- rstudioapi::askForSecret(
      name = 'api_name',
      message = 'Enter your API key here',
      title = "Onboard API Key")
  } else {
    api_key <- readline(prompt = "Enter your Onboard API key:")
  }
  
  Sys.setenv('api_url'=api_url)
  Sys.setenv('api_key'=api_key)
}



#' Access API keys and URL from System Environment
#' @description 
#' 
#' `api.access()` manually retrieves api_url and api_keys stored in the system environment.
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
