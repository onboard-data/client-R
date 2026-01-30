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


# Search Organizations --------------------------------------------------------

#' Search Organizations
#' 
#' @inheritParams orgs
#' @inheritParams verbose
#' 
#' @return A dataframe containing the matched org(s).
search_orgs <- function(orgs = NULL, verbose = TRUE) {
  
  if(is.null(orgs)){
    stop("Please provide 'orgs' paramter.")
  }

    all_orgs <- api.request('organizations', verbose = FALSE)$data
      
      search_text <- paste(orgs, collapse = "|")
      
      result <- all_orgs %>%
        dplyr::filter(
          id %in% orgs |
            grepl(search_text, name, ignore.case = TRUE) |
            grepl(search_text, short_name, ignore.case = TRUE) |
            grepl(search_text, name_abbr, ignore.case = TRUE)
        )
    
      if(verbose){
      if (nrow(result) == 0) {
        cat("No matching orgs found. Please check your input.")
      } else {
        cat(sprintf("Found %d org(s): %s\n", nrow(result), paste(result$name, 
                                                                 collapse = ", ")))
      }
      }
      
  return(result)
}


# Users -------------------------------------------------------------------

#' Users
#' 
#' Retrieve all user info in your organization.
#'
#' @inheritParams orgs
#' 
#' @inheritParams verbose
#' 
#' @return A data.frame of all user information.
#' 
#' @export

get_users <- function(orgs = NULL, verbose = TRUE){
  
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
  convert_to_datetime()  
    
  if(!is.null(orgs)){
    orgs <- search_orgs(orgs = orgs,verbose = verbose)
    
    users <- users %>% 
      filter(org_id %in% orgs$id)
  }
  if(verbose){
  cat(sprintf("Found %s users(s)",nrow(users)))
  }
  return(users)
}


# Deployments -------------------------------------------------------------

#' Deployments
#' 
#' Get all deployments in your organization.
#' 
#' @inheritParams orgs
#' @inheritParams verbose
#' @return A data.frame of all deployments.
#' 
#' @export
get_deployments <- function(orgs = NULL, verbose = TRUE){

  deployments <- api.request('deployment', verbose = FALSE)  %>%
    convert_to_datetime() 
  
  if(!is.null(orgs)){
  orgs <- search_orgs(orgs = orgs,verbose = verbose)
    
  deployments <- deployments %>% 
    filter(org_id %in% orgs$id)
  }
  
  if(verbose){
  cat(sprintf("Found %s deployment(s)",nrow(deployments)))
  }
  
  return(deployments)
}