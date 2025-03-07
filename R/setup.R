#' Set up Onboard API keys and URL in system environment
#' @description
#' 
#' Set the Onboard API URL and API keys in the system environment.
#'  
#' @param api_env Provide the API client name. Default is `prod`
#' 
#' @param url Provide the API URL
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
api.setup <- function(api_env = 'prod', url = '', key = '', token = '',verbose = TRUE) {
  
  if(!(api_env %in% c('prod','dev','rtem','gcp'))){
    stop("Please use 'prod', 'dev', 'gcp' or 'rtem' for api_env")
  }
  Sys.setenv('api_env' = api_env)  

  if (url=="") {
    #Pick url if url is not provided
    url <- dplyr::case_when(
      api_env == 'prod' ~ 'https://api.onboarddata.io',
      api_env == 'dev' ~ 'https:/devapi.onboarddata.io',
      api_env == 'rtem' ~ 'https://api.ny-rtem.com',
      api_env == 'gcp' ~ 'https://rews.onboarddata.io/api'
    )
  }
    Sys.setenv('url' = url)

    Sys.setenv('token' = token)
  
  if(token == "" && key ==""){
  #Get key saved with rstudioapi
      if (Sys.getenv("RSTUDIO") == "1") {
        api_name <- paste0('api_key_', api_env)
        
        key <- rstudioapi::askForSecret(name = api_name,
                                            message = 'Enter your API key here',
                                            title = "Onboard API Key")
      } else {
        key <- readline(prompt = "Enter your Onboard API key:")
      }
  }
      Sys.setenv('key' = key)

  if(verbose){
    cat("Authenticating...\n")
    cat(api.get("whoami")$result)
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
  api_env <- Sys.getenv('api_env')
  url <- Sys.getenv('url')
  key <- Sys.getenv('key')
  token <- Sys.getenv('token')
 
  
  if(url == '' | (key == '' & token== '')){
    stop('API credentials not set correctly.')
  } else {
    return(list(
      'api_env' = api_env,
      'url' = url,
      'key' = key,
      'token' = token,
      headers = add_headers(`Content-Type` = "application/json",
                             Authorization = paste("Bearer", token),
                             `X-OB-Api` = key)
    ))
  }
  

}