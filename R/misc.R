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
  
  pointtypes <- api.get('pointtypes')
  
  measurements <- api.get('measurements')
  
  colnames(measurements) = paste0('measurement_',colnames(measurements))
  
  #Extract units for each measurement
  measurements <- measurements %>% tidyr::unnest(measurement_units,names_sep = "_")
  
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
#' @param data A dataframe to filter
#' 
#' @param org An id (integer) or name (text) of the organization 
#' 
search_by_org <- function(data, org = NULL) {
  
  if(missing(data)){
    stop("data missing.")
  }
  
  if(!("org_name" %in% names(data))){
    orgs <- api.get('organizations', verbose = FALSE)$data %>% 
      select(org_id = id,
             org_name = name)
    
    data <- left_join(data, orgs,by ="org_id") %>% 
      relocate(org_name,.after = org_id)
  }
  
  if (!is.null(org)) {
    if (is.numeric(org)) {
      # If the input is numeric, search by id
      data <- data %>% filter(org_id == org)
      
    } else if (is.character(org)) {
      # If the input is text, search by Name
      data <- data %>% filter(grepl(org, org_name, ignore.case = TRUE))
    } else {
      stop("Invalid input. Please provide either an integer (id) or text (name).")
    }
    
  }
  return(data)
}


# Users -------------------------------------------------------------------

#' Users
#' 
#' Retrieve all user info in your organization.
#' 
#' @param org Enter org_id (integer) or org_name (text)
#' 
#' @return A data.frame of all user information.
#' 
#' @export

get_users <- function(org){
  
  #Get roles db
  roles <- api.get('roles')
  
  roles <- roles$data %>%
    select(roles = .data$id, role_name = .data$name)
  
  #Get user db
  users <- api.get('users')
  
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
                  ~ as_datetime(as.numeric(substr(., 1, 10)),
                                tz = 'UTC'))) 
  
  users <- search_by_org(data = users,org = org)
  
  return(users)
}


# Deployments -------------------------------------------------------------

#' Deployments
#' 
#' Get all deployments in your organization.
#' 
#' @inheritParams get_users
#' 
#' @return A data.frame of all deployments.
#' 
#' @export
get_deployments <- function(org= NULL){

  deployments <- api.get('deployment',verbose = FALSE)
  
  deployments <- deployments %>%
    mutate(across(.data$last_heartbeat,
                  ~ as_datetime(as.numeric(substr(., 1, 10)),
                                tz = 'UTC'))) 
  
deployments <- search_by_org(data = deployments,org = org)
  
  return(deployments)
}