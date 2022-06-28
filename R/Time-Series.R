# Time-Series -------------------------------------------------------------

# api_type='prod'
# 
# api.keys()
# 
# endpoint <- 'timeseries'
# 
# endpoint_url <- paste(api_url, endpoint, sep = '/')
# 
# request_endpoint <- POST(
#   url = endpoint_url,
#   content_type_json(),
#   add_headers(`X-OB-Api` = api_key),
#   # body= '{"start":"2020-11-29T20:16:25Z","end":"2020-11-29T21:16:25Z","point_ids":167500}',
#   body = "{\"start\":\"2020-11-29T20:16:25Z\",\"end\":\"2020-11-30T20:20:25Z\",\"point_ids\":[167500]}"
#   )
# 
# endpoint_db <-
#   content(request_endpoint, as = 'text', encoding = 'UTF-8') %>%
#   fromJSON(flatten = T) %>%
#   as_tibble() %>% 
#   separate(values,c('time','clean','raw','extra'),sep=',c') %>% 
#   mutate(values) %>% 
#   group_by(point_id) %>%
#   do(data.frame(as.list(summary(.$values)), check.names=FALSE)) %>% 
#    unnest(values)
#   #readr::type_convert()
#   mutate(values=gsub('c\\(|\\"|\\)','',values),
#          raw_unit=raw) %>%
#   separate(values,
#            c('time','clean','raw','extra'),
#            sep=',')
# 
# 
# db_buildings_filter <- filter(db_buildings,
#                               grepl('golden',name,ignore.case = T))
# 
# meta_combined <- data.frame()
# 
# for(i in 1:nrow(db_buildings_filter)){
# 
#   building_endpoint <- db_buildings_filter$id[i]
# 
#   api.meta('prod',building_endpoint)
# 
#   assign('single_building',building_db)
# 
#   meta_combined<-rbind(meta_combined,single_building)
# }