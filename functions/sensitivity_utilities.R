



vary_df_each_column_missing<-function(data){
  
  lapply(names(data),function(var_to_remove){
    return(data[,-(which(names(data)==var_to_remove))])
  })
  
  
}



vary_combination_tables <- function(combination_tables){
  combinations<-purrr::map(names(combination_tables),function(pillar){
    this_sector_combinations <- cbind(pillar = pillar,variable_name = colnames(combination_tables[[pillar]]))

  }) %>% do.call(rbind,.) %>% as_tibble
  # don't vary columns that shouldn't be varied
  combinations <- combinations %>% dplyr::filter(!(variable_name %in% c("X","Aggregate","JIAF.CLASSIFICATION")))
  # don't vary columns from sectors with only one column
  combinations<-combinations[combinations$pillar %in% (combinations$pillar %>% table %>% as.data.frame %>% .[.$Freq != 1,"."]),]
  
  
  vary_combination_table_once<-function(pillar,variable_name,combination_tables){
    this_variation<-combination_tables
    this_variation[[pillar]][[variable_name]]<-NULL
    this_variation[[pillar]] <- this_variation[[pillar]] %>% 
      group_by_all(except = c("X","Aggregate","JIAF.CLASSIFICATION")) %>% 
      summarise(JIAF.CLASSIFICATION=mean(JIAF.CLASSIFICATION)) %>% ungroup
    this_variation
  }
  
  
  varied <- combinations %>% purrr::pmap(.f = vary_combination_table_once, combination_tables)
  names(varied)<-combinations$variable_name %>% gsub("si\\.","",.) %>% gsub("\\."," ",.)
  varied
}






group_by_all<-function(data,except = c()){
  data %>% group_by_at(names(data)[-which(names(data)%in%except)]) 
  
}

