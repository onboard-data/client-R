# Some Useful functions to get clean outputs from the databases

# Point Types -------------------------------------------------------------

#' Get Point Types
#' 
#' Queries all point types, measurements and their units from Onboard's Data Model and returns a clean output.
#' 
#' @return A data.frame containing all point types.
#' 
#' @export
get_point_types <- function(){
  
  pointtypes <- api.request(endpoint = 'pointtypes', verbose = FALSE) #Query point_types
  measurements <- api.request(endpoint = 'measurements', verbose = FALSE) #Query measurements
  
  colnames(measurements) <- paste0('measurement_', colnames(measurements))
  
  measurements <- measurements %>%
    tidyr::unnest(measurement_units, names_sep = "_")   #Extract units for each measurement
  
  #Unite data frames
  point_types <- left_join(pointtypes,measurements,
                           by = c("measurement_id"),relationship = "many-to-many") %>%  
    mutate(across(.data$tags,  ~ gsub('\\(|\\)|c|\\"', '', .))) %>%  
    select(.data$id,
           point_type_name = .data$tag_name,
           .data$measurement_name,
           units = .data$measurement_units_name_abbr,
           units_name = .data$measurement_units_name_long,
           data_type = .data$measurement_units_data_type,
           tags = .data$tags)
  
  return(point_types)
  
}


# Search by Org --------------------------------------------------------

#' Search by Org
#' 
#' @param data A data frame containing an `org_id` column. If `org_name` is missing, it will be added via API.
#' @inheritParams org 
#' 
#' @return Filtered data frame with `org_name` included.
search_by_org <- function(data, org = NULL) {
  
  if(missing(data)){
    stop("data missing.")
  }
  
  # Join with org names if missing
  if (!"org_name" %in% names(data)) {
    orgs <- api.request('organizations', verbose = FALSE)$data %>% 
      select(org_id = id, org_name = name)
    
    data <- left_join(data, orgs, by = "org_id") %>%
      relocate(org_name, .after = org_id)
  }
  
  
  if (!is.null(org)) {
    if (is.numeric(org)) {
      # If the input is numeric, search by id
      data <- data %>% filter(org_id == org)
      
    } else if (is.character(org)) {
      # If the input is text, search by Name
      data <- data %>% filter(grepl(org, org_name, ignore.case = TRUE))
    } else {
      stop("Invalid input. Please provide either numeric (id) or character (name).")
    }
    
  }
  return(data)
}


# Users -------------------------------------------------------------------

#' Users
#' 
#' Retrieve all user info in your organization.
#'
#' @inheritParams org
#' 
#' @return A data.frame of all user information.
#' 
#' @export

get_users <- function(org = NULL){
  
  #Get roles db
  roles <- api.request('roles',verbose = FALSE)
  
  roles <- roles$data %>%
    select(roles = .data$id, role_name = .data$name)
  
  #Get user db
  users <- api.request('users', verbose = FALSE)
  
  #Format users
  users <- users$data %>% 
    mutate(across(.data$roles,
                  ~gsub('c\\(|\\)|\\"|','',.))) %>% 
    mutate(across(.data$roles,
                  ~gsub(':',', ',.))) %>% 
    separate_rows(.data$roles,sep = ", ") %>% 
    mutate(across(.data$roles, ~ as.integer(.))) %>% 
    left_join(roles,
              by = c('roles'))  %>%  
    select(.data$id, .data$org_id, .data$org_name, .data$role_name, .data$email, 
           .data$username, .data$first_name, .data$last_name, .data$last_login, 
           .data$created, .data$password_reset, .data$active) %>% 
    #Combine first and last names
    tidyr::unite(col = "name", c(first_name,last_name)) %>% 
      group_by(across(-.data$role_name)) %>%
      summarise(role_name = paste(role_name, collapse = ", "), .groups = "drop") %>%   # Recombine multiple roles
   relocate(role_name,.after=name) %>% 
    mutate(across(c(.data$password_reset, 
                    .data$last_login, .data$created),
                  ~ convert_to_datetime(.))) 
  
  users <- search_by_org(data = users,org = org)
  
  return(users)
}


# Deployments -------------------------------------------------------------

#' Deployments
#' 
#' Get all deployments in your organization.
#' 
#' @inheritParams org
#' 
#' @return A data.frame of all deployments.
#' 
#' @export
get_deployments <- function(org = NULL){

  deployments <- api.request('deployment',verbose = FALSE)
  
  deployments <- deployments %>%
    mutate(across(.data$last_heartbeat,
                  ~ convert_to_datetime(.))) 
  
deployments <- search_by_org(data = deployments, org = org)
  
  return(deployments)
}