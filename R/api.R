
# API Setup ---------------------------------------------------------------

#' Setup API Environment for Onboard Requests
#'
#' Sets authentication and environment variables for making API requests using `httr2`.
#'
#' @param api_env Character. Environment to use. One of `"prod"`, `"dev"`, or `"gcp"`. Defaults to `"prod"`.
#' @param key Character. API key. If not provided, the function prompts for it (via RStudio or console).
#' @param token Character. Optional bearer token.
#' @param verbose Logical. If `TRUE`, prints a confirmation message using a test request.
#'
#' @return Invisibly returns a named list of credentials (api_env, url, key, token).
#' @export
api.setup <- function(api_env = 'prod', 
                      key = NULL, 
                      token = NULL,
                      verbose = TRUE) {
  
  # Validate environment
  valid_envs <- c("prod", "dev", "gcp")
  if (!api_env %in% valid_envs) {
    stop(sprintf("Invalid api_env. Must be one of: %s", paste(valid_envs, collapse = ", ")))
  }

  #Set url based on api_env
    url <- dplyr::case_when(
      api_env == 'prod' ~ "https://api.onboarddata.io/",
      api_env == 'dev' ~ 'https://devapi.onboarddata.io/',
      api_env == 'gcp' ~ 'https://rews.onboarddata.io/api/'
    )
  
    # Retrieve key if not provided
    if (is.null(token) && is.null(key)) {
      #Get key saved with rstudioapi
      if (Sys.getenv("RSTUDIO") == "1") {
        api_name <- paste0("api_key_", api_env)
        key <- rstudioapi::askForSecret(name = api_name,
                                        message = "Enter your API key here",
                                        title = "Onboard API Key")
      } else {
        key <- readline(prompt = "Enter your Onboard API key: ")
      }
    }
    
    # Store credentials in environment variables
    Sys.setenv(api_env = api_env)
    Sys.setenv(url = url)
    
    if (is.null(token)) {
      Sys.unsetenv("token")
    } else {
      Sys.setenv(token = token)
    }
    
    if (is.null(key)) {
      Sys.unsetenv("key")
    } else {
      Sys.setenv(key = key)
    }
    
    
    # Optional test and welcome
    if (verbose) {
      cat("Authenticating...\n")
      whoami <- api.request(endpoint = "whoami", verbose = FALSE)
      cat(sprintf("Welcome %s!\n", whoami$userInfo$name))
    }
    
    invisible(list(
      env = api_env,
      url = url,
      key = key,
      token = token
    ))
    
}

#' Access stored API credentials
#'
#' Retrieves API environment settings and credentials from the system environment.
#' Used internally by other functions to authenticate API requests.
#'
#' @return A list containing `api_env`, `url`, `key`, and `token`.
api.access <- function(){
  
  # Retrieve environment variables
  api_env <- Sys.getenv("api_env", unset = NA)
  url     <- Sys.getenv("url", unset = NA)
  key     <- Sys.getenv("key", unset = "")
  token   <- Sys.getenv("token", unset = "")
  
  # Validate required fields
  missing <- c(
    if (is.na(url) || url == "") "url",
    if (key == "" && token == "") "key/token"
  )
 
  if (length(missing) > 0) {
    stop(sprintf("Missing or invalid API credentials: %s. Run `api.setup()` first.", 
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  
  # Return credentials
  list(
    api_env = api_env,
    url     = url,
    key     = key,
    token   = token
  )
  

}


# API Request -------------------------------------------------------------

#' Make an API Request with httr2
#'
#' A general-purpose function to make authenticated API requests to Onboard's API. Supports multiple HTTP methods, JSON request bodies, and file uploads.
#'
#' @param endpoint A character string specifying the API endpoint (appended to the base URL).
#' @param method A character string specifying the HTTP method to use. One of `"GET"`, `"POST"`, `"PATCH"`, or `"DELETE"`. Defaults to `"GET"`.
#' @param request_body An optional list or named object to send as JSON in the request body.
#' @param file An optional character string specifying the path to a file for upload. Used only with `"POST"` requests.
#' @param response_body A character string indicating how to parse the response. One of `"string"` or `"json"`. Defaults to `"string"`
#' @inheritParams verbose
#'
#' @return A parsed API response. If `response_body = "string"`, the response is converted to a list or dataframe. If `"json"`, returns the raw JSON structure as a list.
#'
#' @details
#' This function wraps `httr2` functionality for making authenticated requests to Onboard’s API.
#' It uses credentials retrieved from `api.access()` function that must return a named list with elements: `url`, `key`, and `token`.
#'
#' If both `file` and `request_body` are provided, the function will raise an error to avoid ambiguity in the request body.
#'
#' @export
api.request <- function(endpoint,
                        method = "GET",
                        request_body = NULL,
                        file = NULL,
                        response_body = "string",
                        verbose = TRUE){
  
  #Allowed methods
  allowed_methods <- c("GET","POST","PATCH","DELETE")
  
  if(!method %in% allowed_methods){
    stop(sprintf("method must be one of %s.",paste(allowed_methods,collapse=", ")))
  }
  
  #ALlowed reponse_body
  allowed_reponse_body <- c("string","json")
  
  if(!response_body %in% allowed_reponse_body){
    stop(sprintf("response_body must be one of %s.", paste(allowed_reponse_body, collapse = ", ")))
  }
  
  if (!is.null(file) && !is.null(request_body)) {
    stop("You cannot specify both `file` and `request_body`. Choose one.")
  }
  
  # Access API credentials
  api_data <- api.access()
  
  # Construct the endpoint URL
  endpoint_url <- paste0(api_data$url, endpoint)
  
  # Create the request
  req <- request(endpoint_url) %>%
    req_headers("x-ob-api" = api_data$key,
                "Authorization" = paste("Bearer", api_data$token)) %>% 
    req_method(method = method)
  
  
  if (!is.null(file)) {
    # POST with file upload
    req <- req %>% req_body_multipart(file = curl::form_file(file))
  }
  
  if (!is.null(request_body)) {
    # Regular POST with JSON body
    req <- req %>% req_body_json(request_body)
  }
  
  if(verbose){
    req <- req %>% req_verbose(body_resp = TRUE, redact_headers = FALSE) 
  }
  
  api_response <- req %>%  
    req_perform()
  
  # Parse the response
  if(response_body == "string"){
    api_output <- api_response %>%
      resp_body_string()  %>% 
      fromJSON(flatten = TRUE)
  } else if (response_body == "json"){
    api_output <- api_response %>% 
      resp_body_json()
  }
  
  return(api_output)
  
}
