# Raw Timeseries Data ------------------------------------------------

#' Raw Time-Series Data
#' 
#' Retrieves timeseries data in raw format
#' 
#' @param start_time: Start Time in UTC
#' @param end_time: End Time in UTC
#' @param point_ids: Point IDs for which timeseries data needs to be queried
#' @export
get_timeseries_raw <- function(start_time,end_time,
                              point_ids){

  start_time <- as.numeric(as.POSIXlt(start_time))
  
  end_time <- as.numeric(as.POSIXlt(end_time))
  
  timeseries_query <- list(start=start_time,
                           end=end_time,
                           point_ids=point_ids) %>%
    toJSON()
  
  # Format JSON query
  timeseries_query <- gsub('start":\\[','start":',timeseries_query)
  timeseries_query <- gsub('\\],"end":\\[',',"end":',timeseries_query)
  timeseries_query <- gsub('\\],"point',',"point',timeseries_query)
  
  timeseries_output <- api.post(endpoint='timeseries',
                                json_body=timeseries_query,
                                output = 'list')
  
  if(length(timeseries_output)!=0){
    
  timeseries_df <- data.frame()
  
  for (i in 1:length(timeseries_output)){
    
    single_output <- timeseries_output[[i]]
    
    point_id <- single_output[['point_id']]
    
    ts_single <- rrapply::rrapply(single_output,how='melt') %>% 
      filter(!grepl('raw|unit|topic|columns|display',L1)) %>% 
      mutate(L1=point_id) %>%
      filter(!is.na(L2)) %>% 
      mutate(across(L3, ~ ifelse(. == 1,'timestamp',
                                 ifelse(. == 2,'raw',
                                        ifelse(. == 3, 'unit',
                                               .))))) %>%   
      tidyr::pivot_wider(id_cols = c(1:2),
                  names_from=L3,
                  values_from = value) %>%
      select(-L2) %>%
      rename('point_id'=L1)
    
    timeseries_df <- plyr::rbind.fill(timeseries_df,ts_single)
    
  }
  
  if(4 %in% names(timeseries_df)){
    timeseries_df <- mutate(timeseries_df,
                            raw=`4`) %>% 
      select(-`4`)
  }
  } else {
    timeseries_df <- data.frame()
    print('No timeseries data found.')
  }
  
  return(timeseries_df)
}



# Clean timeseries --------------------------------------------------------

#' Time-Series Data
#' 
#' Clean Time-Series Output
#' 
#' @inheritParams get_timeseries_raw
#' 
#' @export
get_timeseries <- function(start_time,end_time,point_ids){
  
  timeseries_raw <- get_timeseries_raw(start_time = start_time,
                                       end_time = end_time,
                                       point_ids = point_ids)
  if(nrow(timeseries_raw)!=0){
    timeseries_clean <- timeseries_raw %>%
      transmute(timestamp,
                point_id,
                unit=as.character(unit))  %>% 
      tidyr::pivot_wider(id_cols = timestamp,
                  names_from=point_id,
                  values_from=unit,
                  values_fill = NA) %>% 
      mutate(across(timestamp,
                    ~gsub('[.].*','',.))) %>%
      type.convert(as.is=T) %>% 
      mutate(timestamp= as_datetime(timestamp))
  } else {
    timeseries_clean <- timeseries_raw
  }
  
}
