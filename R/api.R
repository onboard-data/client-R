# API Setup ---------------------------------------------------------------

# Environment -> base URL lookup used by api.setup(). Single source of
# truth for both the valid `api_env` values and their URLs.
.onboard_api_urls <- c(
  prod = "https://api.onboarddata.io/",
  dev  = "https://devapi.onboarddata.io/",
  gcp  = "https://rews.onboarddata.io/api/"
)

#' Resolve an api_env to its Base URL
#'
#' Internal helper for `api.setup()`. Validates `api_env` against the known
#' Onboard environments and returns the corresponding base URL.
#'
#' @param api_env Character. One of `"prod"`, `"dev"`, or `"gcp"`.
#'
#' @return Character. The base URL for the given environment.
.api_env_url <- function(api_env) {
  if (!api_env %in% names(.onboard_api_urls)) {
    stop(sprintf("Invalid api_env. Must be one of: %s",
                 paste(names(.onboard_api_urls), collapse = ", ")))
  }
  unname(.onboard_api_urls[api_env])
}

#' Prompt the User for an API Key
#'
#' Internal helper for `api.setup()`. Uses `rstudioapi::askForSecret()` when
#' running inside RStudio (so the key can optionally be saved with keyring),
#' otherwise falls back to a plain console prompt.
#'
#' @param api_env Character. Used to namespace the saved RStudio secret
#'   (e.g. `"api_key_prod"`).
#'
#' @return Character. The entered API key.
.prompt_for_api_key <- function(api_env) {
  if (Sys.getenv("RSTUDIO") == "1") {
    rstudioapi::askForSecret(
      name = paste0("api_key_", api_env),
      message = "Enter your API key here",
      title = "Onboard API Key"
    )
  } else {
    readline(prompt = "Enter your Onboard API key: ")
  }
}

#' Set or Unset a System Environment Variable
#'
#' Internal helper for `api.setup()`. Captures the repeated
#' "set if non-NULL, otherwise unset" pattern used for both `key` and `token`.
#'
#' @param name Character. Name of the environment variable.
#' @param value Character or NULL. Value to set; if NULL the variable is unset.
.set_or_unset_env <- function(name, value) {
  if (is.null(value)) {
    Sys.unsetenv(name)
  } else {
    do.call(Sys.setenv, setNames(list(value), name))
  }
}

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

  url <- .api_env_url(api_env)

  # Retrieve key if not provided
  if (is.null(token) && is.null(key)) {
    key <- .prompt_for_api_key(api_env)
  }

  # Store credentials in environment variables
  Sys.setenv(api_env = api_env)
  Sys.setenv(url = url)
  .set_or_unset_env("token", token)
  .set_or_unset_env("key", key)

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

#' Validate Stored API Credentials
#'
#' Internal helper for `api.access()`. Stops with a descriptive error if
#' required credentials are missing.
#'
#' @param url Character or NA. Stored base URL.
#' @param key Character. Stored API key (empty string if unset).
#' @param token Character. Stored bearer token (empty string if unset).
.validate_credentials <- function(url, key, token) {
  missing <- c(
    if (is.na(url) || url == "") "url",
    if (key == "" && token == "") "key/token"
  )

  if (length(missing) > 0) {
    stop(sprintf("Missing or invalid API credentials: %s. Run `api.setup()` first.",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
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

  .validate_credentials(url, key, token)

  # Return credentials
  list(
    api_env = api_env,
    url     = url,
    key     = key,
    token   = token
  )

}


# API Request -------------------------------------------------------------

.allowed_methods <- c("GET", "POST", "PATCH", "DELETE")
.allowed_response_bodies <- c("string", "json")

#' Validate api.request() Arguments
#'
#' Internal helper for `api.request()`. Checks `method` and `response_body`
#' against their allowed values, and that `file`/`request_body` aren't both set.
#'
#' @param method Character. HTTP method.
#' @param response_body Character. Response parsing mode.
#' @param file Character or NULL. Path to a file for upload.
#' @param request_body List or NULL. JSON request body.
.validate_request_args <- function(method, response_body, file, request_body) {
  if (!method %in% .allowed_methods) {
    stop(sprintf("method must be one of %s.", paste(.allowed_methods, collapse = ", ")))
  }

  if (!response_body %in% .allowed_response_bodies) {
    stop(sprintf("response_body must be one of %s.", paste(.allowed_response_bodies, collapse = ", ")))
  }

  if (!is.null(file) && !is.null(request_body)) {
    stop("You cannot specify both `file` and `request_body`. Choose one.")
  }
}

#' Build an httr2 Request for the Onboard API
#'
#' Internal helper for `api.request()`. Assembles headers, method, body
#' (file upload or JSON), and optional verbose logging onto an httr2 request.
#'
#' @param endpoint_url Character. Full request URL.
#' @param api_data List. Credentials from `api.access()` (`key`, `token`).
#' @param method Character. HTTP method.
#' @param file Character or NULL. Path to a file for upload.
#' @param request_body List or NULL. JSON request body.
#' @param verbose Logical. Whether to attach `req_verbose()`.
#'
#' @return An httr2 request object, ready for `req_perform()`.
.build_api_request <- function(endpoint_url, api_data, method, file, request_body, verbose) {
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

  if (verbose) {
    req <- req %>% req_verbose(body_resp = TRUE, redact_headers = FALSE)
  }

  req
}

#' Parse an httr2 API Response
#'
#' Internal helper for `api.request()`. For `response_body = "json"`, parses
#' with `resp_body_json()`. For `"string"`, reads the body as a string and
#' sniffs whether it looks like JSON or CSV, parsing accordingly (falling
#' back to the raw string otherwise).
#'
#' @param api_response An httr2 response object.
#' @param response_body Character. One of `"string"` or `"json"`.
#'
#' @return A parsed API response (list, data.frame, or character string).
.parse_api_response <- function(api_response, response_body) {
  if (response_body == "json") {
    return(api_response %>% resp_body_json())
  }

  api_output <- api_response %>% resp_body_string()

  is_json <- grepl("^\\s*\\{", api_output) || grepl("^\\s*\\[", api_output)
  is_csv  <- grepl(",", api_output) && grepl("\n", api_output)

  if (is_json) {
    api_output <- jsonlite::fromJSON(api_output)
  } else if (is_csv) {
    api_output <- read.csv(text = api_output, stringsAsFactors = FALSE)
  }

  api_output
}

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

  .validate_request_args(method, response_body, file, request_body)

  # Access API credentials
  api_data <- api.access()

  # Construct the endpoint URL
  endpoint_url <- paste0(api_data$url, endpoint)

  # Create the request
  req <- .build_api_request(endpoint_url, api_data, method, file, request_body, verbose)

  api_response <- req %>%
    req_perform()

  .parse_api_response(api_response, response_body)

}
