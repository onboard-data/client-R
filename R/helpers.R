
# UNIX to POSIXct ---------------------------------------------------------

#' Convert UNIX timestamp to POSIXct
#' 
#' Converts a UNIX timestamp string (with or without milliseconds) to a POSIXct datetime object.
#' 
#' @param x A character or numeric vector of UNIX timestamps.
#' 
#' @return A POSIXct vector.
convert_to_datetime <- function(x) {
  x %>%
    mutate(across(
      any_of(
        c("created","modified","last_discovery","last_published",
          "last_updated","first_updated","last_heartbeat","password_reset","last_login")),
      ~ {
        suppressWarnings(
          dplyr::case_when(
          # Detect epoch timestamps
          grepl("^\\d{10,16}", .) ~ 
            as_datetime(as.numeric(.) / ifelse(nchar(.) > 10, 1000, 1), 
                        tz = Sys.timezone()),
          
          is.na(.) | . == "" ~ NA,
          
          # Otherwise try parsing standard datetime strings
          TRUE ~ lubridate::ymd_hms(., quiet = TRUE, tz = Sys.timezone())))
      }
      ))
  
}

# Nested List to DF -------------------------------------------------------

#' Nested List to DF
#' 
#' Vectorized approach to convert a deeply nested into a dataframe. This function is customized only to work with time-series output. Used in `get_timeseries_raw`
#' 
#' @param nested_list time-series output return from /timeseries API endpoint
#' 
#' @return Clean dataframe

nested_list_to_df <- function(nested_list) {
  
  item <- nested_list[[1]]
  
  # Use map to iterate over each list element and process
  df_list <- purrr::map(nested_list, function(item) {
    # Extract data
    point_id <- item$point_id
    topic <- item$topic
    display <- item$display
    columns <- item$columns
    values <- item$values
    #Check if attribute "clean" exists in columns list and remove it from columns list and values list as well. This is present in older time-series data
    if("clean" %in% columns){ 
    #Get index of "clean" in columns
    clean_index <- which(columns=="clean")
    
    columns <- columns[-clean_index]
    
    values <- lapply(values,function(x) x[-clean_index])
    }
    
    #If "raw" attribute exists in column, convert 3rd name in columns to "unit" else convert 2nd name in columns to "unit"

    if("raw" %in% columns){
    columns[[3]] <- "unit"
    } else{
      columns[[2]] <- "unit"
    }
    
    # Convert the values list into a data frame
    values_df <- as.data.frame(do.call(rbind, values))
    
    # Set the column names from the 'columns' field
    colnames(values_df) <- columns
    
    # Add point_id, topic, and display as new columns
    values_df <- values_df %>%
      mutate(point_id = point_id, 
             topic = topic, 
             display = display)
    
    return(values_df)
  })
  
  # Combine all data frames in the list using list_rbind from purrr
  df_combined <- purrr::list_rbind(df_list)
  
  return(df_combined)
}