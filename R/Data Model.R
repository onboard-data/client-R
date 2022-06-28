# Query Data Model --------------------------------------------------------

##Query All Equipment Types
get_equip_types <- function(){
  
  equiptype <-api.get('equiptype')
  
  #Get subtypes
  subtype<-rrapply(equiptype$sub_types,how='melt') %>% 
    mutate_at(vars(value),
              ~gsub('c\\(|\\(|\\"|\\)','',.))  
  
  char_split<-data.frame(str_split(subtype$value,', ',n=Inf,simplify=T))
  
  subtype_format <- bind_cols(subtype,char_split) %>% 
    select(-value) %>% 
    filter(L2!='id')  %>% 
    pivot_longer(cols = c(-1,-2),names_to = 'names',values_to = 'values') %>% 
    filter(values!='') %>% 
    pivot_wider(id=c(L1,names),names_from = L2,values_from = 'values') %>% 
    select(-c(L1,names)) %>% 
    mutate_at(vars(equipment_type_id),~as.integer(.))
  
  equip_types <- equiptype %>% 
    filter(active==T) %>% 
    select(-c(sub_types,critical_point_types,flow_order,active)) %>% 
    left_join(subtype_format,by=c('id'='equipment_type_id'),
              suffix = c('','_subtype'))
  
  return(equip_types)
}

## Get Point types, measurements and their units in a clean output

get_point_types <- function(){
  
  pointtypes <- api.get('pointtypes')
  
  units <- api.get('unit')
  
  measurements <- api.get('measurements')  
  
  # Get measurements and their associated units
  measurement_unit_df <- data.frame()
  
  for (i in 1:nrow(measurements)){
    row_num =i
    
    measurement_single <- measurements[row_num,]
    
    
    unit_list <- measurement_single$units 
    
    if(!nrow(unit_list[[1]])==0){
      
      
      unit  <-  data.frame(t(sapply(unit_list,c))) %>% 
        select(unit_name=name_long,unit=name_abbr,data_type) %>% 
        mutate_all(~gsub('c\\("|"|\\)','',.)) %>% 
        mutate(name=measurement_single$name) 
      
      measurement_unit_df_single <- left_join(measurement_single,unit,
                                              by=c('name')) %>% 
        select(id,measurement_name=name,
               units_convertible,
               qudt_type,
               unit_name,
               unit,
               data_type)
      
      measurement_unit_df <- rbind(measurement_unit_df,measurement_unit_df_single)
    }
  }
  
  #Unite data frames
  point_types <- left_join(select(pointtypes,id,tag_name,measurement_id,tags),
                           measurement_unit_df,
                           by = c("measurement_id" = 'id')) %>% 
    mutate_at(vars(tags),  ~ gsub('c\\(|\\)', '', .)) %>% 
    select(id,
           point_type = tag_name,
           measurement_name,
           unit,
           data_type,
           tags)
  
  return(point_types)
  
}
