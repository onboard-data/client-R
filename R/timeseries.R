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
  print(sprintf("Querying time-series data from %s to %s for %s points...\n",
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
        
        timeseries_query <- append(timeseries_query,
                                   list(units=units)) 
      }
    }
  
  timeseries_query <- jsonlite::toJSON(timeseries_query)
  
  # Format JSON query
  . <- NULL
  timeseries_query <- timeseries_query %>% 
    gsub('start":\\[', 'start":', .) %>% 
    gsub('\\],"end":\\[', ',"end":', .) %>% 
    gsub('\\],"point', ',"point', .) %>% 
    gsub('units":\\[', 'units":', .) %>% 
    gsub('}\\]}', '}}', .)
  
  timeseries_output <- api.post(endpoint = 'timeseries',
                                json_body = timeseries_query)
  
  if(length(timeseries_output)!=0){
    timeseries_df <-  nested_list_to_df(timeseries_output)

  } else {
    timeseries_df <- data.frame()
    print('No timeseries data found.')
  }
  
  return(timeseries_df)
}


# Clean timeseries --------------------------------------------------------

#' Time-Series Data
#' 
#' Provides clean time-series
#' 
#' @inheritParams get_timeseries_raw
#' 
#' @param unit_type Provide the unit type: "default" or "raw"
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
    
    if(unit_type == 'default'){
    timeseries_clean <- timeseries_raw %>% 
      transmute(.data$time,
                .data$display,
                unit = as.character(.data$unit))
    
    } else if (unit_type == 'raw'){
      timeseries_clean <- timeseries_raw %>% 
        transmute(.data$time,
                  .data$display,
                  unit = as.character(.data$raw))
    }
    
    timeseries_clean <- timeseries_clean %>% 
      mutate(across(.data$time,
                    ~gsub('[.].*','',.))) %>%
      type.convert(as.is = T) %>% 
      mutate(time = as_datetime(.data$time)) %>% 
      pivot_wider(id_cols = .data$time,
                  names_from = .data$display,
                  values_from = .data$unit,
                  values_fill = NA) 
    
    return(timeseries_clean)
  } 
