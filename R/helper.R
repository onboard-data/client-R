

# Nested List to DF -------------------------------------------------------

#' Nested List to DF
#' 
#' Vectorized approach to convert a deeply nested into a dataframe. This function is customized only to work with time-series output. Used in `get_timeseries_raw`
#' 
#' @param nested_list time-series output return from /timeseries API endpoint
#' 
#' @return Clean dataframe

nested_list_to_df <- function(nested_list) {
  # Use map to iterate over each list element and process
  df_list <- purrr::map(nested_list, function(item) {
    # Extract point-specific data
    point_id <- item$point_id
    topic <- item$topic
    display <- item$display
    columns <- item$columns
    #Convert 3rd name in columns to "unit"
    columns[[3]] <- "unit"
    
    # Convert the values list into a data frame
    values_df <- as_tibble(do.call(rbind, item$values))
    
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


# Process Columns ---------------------------------------------------------

#' Process Columns
#' 
#' Deprecated. Function to process all columns in a dataframe inside a list. Converts list type columns to character. Used in `get_staged_data` 
#' 
#' @param df dataframe to parse
#' 
#' @return Processed df
#' 
process_columns <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.list(col)) {
      sapply(col, function(x) paste(unlist(x), collapse = ", "))
    } else {
      as.character(col)
    }
  })
  return(df)
}
