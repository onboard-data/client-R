# Staging Area API --------------------------------------------------------

##Get metadata from staging area
get_staged_data <- function(building_id){
  
  get_building_info(building_id)
  
  endpoint <- paste0('staging/',building_id,'?points=True')
  
  stage <- api.get(endpoint)
  
  #Equip Data
  
  equip_data<- stage$equipment
  equip_data_names <- names(equip_data)
  equip_data_names <- gsub('data\\.','',equip_data_names)
  equip_data_names <- paste0('e.',equip_data_names)
  names(equip_data) <- equip_data_names
  
  equip_data_names <- data.frame(names=names(equip_data)) %>% 
    filter(!grepl('confidences|\\.cnf|auto_tagger|\\.err|type_id|tags|created',
                  names))  
  
  equip_data <- equip_data %>% 
    select(equip_data_names$names)
  
  #Points Dara
  
  points_list <- stage$points_by_equip_id
  
  points_data <- rbindlist(points_list,fill=T)
  
  points_data_names <- names(points_data)
  points_data_names <- gsub('data\\.','',points_data_names)
  points_data_names <- paste0('p.',points_data_names)
  names(points_data) <- points_data_names
  
  rem_col <- paste('auto_tagger','.cnf','polarity','reliability',
                   'activeText','event','covIncrement','presvalue',
                   'statusflags','outofservice','unit_id','type_id',
                   'datasource','limit','deadband','@prop','timedelay',
                   'notif','acked','resolution','state_text',
                   'relinquish','priority','p\\.e\\.','confidences',
                   'check','created',sep='|')
  
  points_data_names <- data.frame(names=points_data_names) %>% 
    filter(!grepl(rem_col,names,ignore.case=T))
  
  points_data<- points_data %>% 
    select(points_data_names$names)
  
  
  staged_data <- left_join(equip_data,
                           points_data,
                           by=c('e.equip_id'='p.equip_id')) %>% 
    mutate_at(vars(e.last_promoted,p.last_promoted,e.modified,p.modified),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York')) %>%
    select(sort(tidyselect::peek_vars()))
  
  return(staged_data)
  
} 

##Upload data to the staging area or assign __SKIP__ equip_id to topics on the staging area
##skip_topics is optional (T or F)(Use with Caution)

upload_staging <- function(building,
                           data_to_upload,
                           skip_topics){
  
  get_building_info(building)
  
  if (missing(skip_topics)){
    skip_topics <- F
  }
  
  if(missing(data_to_upload)) {
    
    stop('data_to_upload is missing in the function call. data_to_upload should be a dataframe including atleast e.equip_id & p.topic for the upload to succeed')
    
  } else if (!('p.topic' %in% names(data_to_upload))) {
    print('p.topic column not found in staging_upload.')
    
  } else if (!('e.equip_id' %in% names(data_to_upload)) &
             skip_topics == F) {
    stop('e.equip_id column not found in data_to_upload.')
  } else if (skip_topics ==T){
    data_to_upload <- data_to_upload %>% 
      transmute(e.equip_id = '__SKIP__', p.topic)
    
    operation <- 'Skipping'
  } else {
    operation <- 'Uploading'
  }
  
  data_to_upload_json <- data_to_upload %>% 
    toJSON()
  
  proceed <- askYesNo(sprintf('Do you want to proceed %s %s topics for %s?',operation,nrow(data_to_upload),name))
  
  if(proceed!=T){
    stop('Stopping Operation.')
  }
  
  print(sprintf('%s topics...',operation))
  
  #get endpoint
  endpoint <- paste0('staging/',id)
  
 post_points <- api.post(endpoint,json_body = data_to_upload_json)
  
 if(length(post_points$row_errors)==0){
   print('Success!')
 } else{
   return(post_points)
 }
 
}

##Promote valid data on the staging area to the live building 

promote_staged_data <- function(building,
                                data_to_promote){
  
  get_building_info(building)
  
  if(missing(data_to_promote)){
    
    proceed <- askYesNo(sprintf('Do you want to proceed with promoting all valid topics for %s?',name))
    
    if(proceed==T){
      
      promote_json <- list(equip_ids='',topics='') %>% toJSON()
      
      promote_json <- gsub('\\["','[',promote_json)
      promote_json <- gsub('"\\]',']',promote_json)
      
      operation <- 'promote_all'
      
    } else {
      stop('Stopping Operation.')
      
    }} else if (!('p.topic' %in% names(data_to_promote))) {
      print('p.topic column not found in data_to_promote.')
      
    } else if (!('e.equip_id' %in% names(data_to_promote))) {
      stop('e.equip_id column not found in data_to_promote')
    } else {
      data_to_promote <- data_to_promote %>% 
        filter(e.equip_id!='__SKIP__')
      
      equip_count <-length(unique(data_to_promote$e.equip_id))
      
      proceed <-
        askYesNo(
          sprintf(
            'Do you want to proceed with promoting %s equipment and their valid topics to %s?',equip_count,name))
      
      if(proceed==T){
        
        promote_json <- list(equip_ids=data_to_promote$e.equip_id,topics=data_to_promote$p.topic) %>%  
          toJSON()
        
        operation <- 'promote_some'
      } else{
        stop('Stopping Operation.')
      }
    }
  
  endpoint <- paste0('staging/',id,'/apply')
  
  promote_data <- api.post(endpoint,
                           json_body = promote_json)  
  
  
validation_errors <-rrapply(promote_data,how='melt') %>% 
  filter(L1!='building_id')

if(nrow(validation_errors)!=0){
  
  validation_errors <- validation_errors %>% 
  filter(L2!='__SKIP__',!is.na(L2))
   
      assign('validation_errors',
             validation_errors,
             parent.frame())
      
      stop('See validation_errors.')
    }
    else {
      print('Passed Validation.')
    }
  
}

