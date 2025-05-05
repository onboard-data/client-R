# Raw Timeseries Data ------------------------------------------------

#' GET Raw Time-Series Data
#' Retrieves raw-format time series data for specified point IDs and time range.
#' @param start_time Start time in UTC.
#' @param end_time End time in UTC.
#' @param point_ids Numeric. Vector of point IDs to query.
#' @param units Optional list of preferred units for measurements.
#' @inheritParams verbose
#' @return A long-format data.frame with point ID, timestamp, and raw values.
#' @examples
#' \dontrun{
#' 
#' end_time <- as.POSIXct(Sys.time(), tz = 'UTC')
#' 
#' start_time <- end_time - lubridate::hours(6)
#' 
#' point_ids <- c(290631, 290643) 
#' 
#' units <- list("temperature" = "k")
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
  
  if (length(point_ids) == 1) {
    point_ids <- list(point_ids)
  }
  
  timeseries_query <- list(
    start = as.numeric(as.POSIXlt(start_time, tz = "UTC")),
    end = as.numeric(as.POSIXlt(end_time, tz = "UTC")),
    point_ids = point_ids
  )
  
  
  if (!is.null(units)) {
    if (!is.list(units)) stop("units must be a list.")
    timeseries_query$units <- units
  }
  
  result <- api.request(
    endpoint = "timeseries",
    method = "POST",
    request_body = timeseries_query,
    response_body = "json",
    verbose = verbose
  )
  
  if (is.null(result$status)) {
    nested_list_to_df(result)
  } else {
    if (verbose) print(result)
    invisible(NULL)
  }

}


# Clean timeseries --------------------------------------------------------

#' Time-Series Data
#' Returns cleaned, wide-format time-series data with one column per point ID.
#' @inheritParams get_timeseries_raw
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
#' units <- list("temperature" = "k")
#' 
#' timeseries <- get_timeseries(start_time, end_time, point_ids, units)
#' 
#' }
#' 
#' @export
get_timeseries <- function(start_time, end_time, point_ids, units = NULL, verbose = TRUE){

  timeseries_raw <- get_timeseries_raw(start_time = start_time,
                                       end_time = end_time,
                                       point_ids = point_ids,
                                       units = units,
                                       verbose = verbose)
    
  if(nrow(timeseries_raw)==0){
    timeseries <- timeseries_raw
  } else {
   
    timeseries <- timeseries_raw %>% 
      transmute(.data$time,
                .data$display,
                values = as.character(.data$unit))
    
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