# Raw Timeseries Data ------------------------------------------------

#' Raw Time-Series Data
#' 
#' Retrieves timeseries data in raw format.
#' 
#' @param start_time Start Time in UTC.
#' @param end_time End Time in UTC.
#' @param point_ids Point IDs for which timeseries data needs to be queried.
#' @param units (Optional) A data.frame consisting of preferred units for given measurements
#' 
#' @return A long data.frame of time series data, with point id, timestamp, and raw point values as columns.
#' 
#' @examples
#' \dontrun{
#' 
#' end_time <- as.POSIXlt(Sys.time(), tz = 'UTC')
#' 
#' start_time <- end_time - lubridate::hours(6)
#' 
#' point_ids <- c(290631, 290643) 
#' 
#' units <- data.frame("temperature" = "k")
#' 
#' timeseries <- get_timeseries_raw(start_time, end_time, point_ids, units)
#' 
#' }
#' 
#' @export
get_timeseries_raw <- function(start_time, end_time, point_ids, units = NULL, 
                               verbose = TRUE){
  
  if(verbose){
  cat(sprintf("Querying time-series data from %s to %s for %s points...\n",
                start_time, end_time, length(point_ids)))
}
  start_time <- as.numeric(as.POSIXlt(start_time, tz = 'UTC'))
  
  end_time <- as.numeric(as.POSIXlt(end_time, tz = 'UTC'))
  
  timeseries_query <- list(start = start_time,
                           end = end_time,
                           point_ids = point_ids)
    
    if(!is.null(units)){
    
      if(class(units) != "data.frame"){
         stop("units must be a dataframe.")
        
      } else {
        
        timeseries_query$units <- as.list(units)
      }
    }
  
  timeseries_query_json <- jsonlite::toJSON(timeseries_query,auto_unbox = TRUE)

  
  timeseries_output <- api.post(endpoint = 'timeseries',
                                json_body = timeseries_query_json)
  
  
  if (is.null(timeseries_output$status)) {

    timeseries_output <-  nested_list_to_df(timeseries_output)
    
    return(timeseries_output)
  } else {
    if(verbose){
      print(timeseries_output)
    }
  }

}


# Clean timeseries --------------------------------------------------------

#' Time-Series Data
#' 
#' Provides clean time-series
#' 
#' @inheritParams get_timeseries_raw
#' 
#' @param unit_type Provide the unit type: "default" (default) or "raw"
#' 
#' @return A wide data.frame of time-series data, with timestamp and all requested point IDs as columns.
#' 
#' @examples
#' \dontrun{
#' 
#' end_time <- as.POSIXlt(Sys.time(), tz = 'UTC')
#' 
#' start_time <- end_time - lubridate::hours(6)
#' 
#' point_ids <- c(290631, 290643) 
#' 
#' units <- data.frame("temperature" = "k")
#' 
#' timeseries <- get_timeseries(start_time, end_time, point_ids, units)
#' 
#' }
#' 
#' @export
get_timeseries <- function(start_time, end_time, point_ids, units = NULL,
                           unit_type = 'default'){

  timeseries_raw <- get_timeseries_raw(start_time = start_time,
                                       end_time = end_time,
                                       point_ids = point_ids,
                                       units = units)
    
  if(nrow(timeseries_raw)==0){
    timeseries <- timeseries_raw
  } else {
    if(unit_type == 'default'){
    timeseries <- timeseries_raw %>% 
      transmute(.data$time,
                .data$display,
                values = as.character(.data$unit))
    
    } else if (unit_type == 'raw'){
      timeseries <- timeseries_raw %>% 
        transmute(.data$time,
                  .data$display,
                  values = as.character(.data$raw)) 
    }
    
    timeseries <- timeseries %>% 
      mutate(across(time, ~ format(as.POSIXct(as.character(.), 
                                                    format = "%Y-%m-%dT%H:%M:%OSZ", 
                                                    tz = "UTC"), 
                                         "%Y-%m-%d %H:%M:%S"))) %>% 
      distinct(time, display, .keep_all = TRUE) %>% 
      pivot_wider(id_cols = time,
                  names_from = display,
                  values_from = values,
                  values_fill = NA) 
}

    return(timeseries)
} 