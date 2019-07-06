



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









#' take original msni score + table of variations of msni score; calculate hypothesis tests if difference larger than threshold; return nice table
sensitivity_variation_test_result_table<-function(msni_original,msni_varied,threshold, strata_data_name, strata_data_values){
  
  msni_diffs <- msni_varied %>% purrr::map(function(x){abs(x-msni_original)-threshold}) %>% as_tibble
  names(msni_diffs)<-paste0("diff",1:ncol(msni_diffs))
  msni_diffs<- lapply(msni_diffs,unlist) %>% as.data.frame
  msni_diffs[[strata_data_name]]<-strata_data_values
  
  # make survey design object
  design<-map_to_design(msni_diffs,
                        weighting_function = weighting)
  
  # define test formulas:
  test_formulas<-sapply(paste0(names(msni_diffs)[names(msni_diffs)!="camp_location"],"~",0),formula)
  
  # run tests:
  tests<-lapply(test_formulas,svyttest,design = design)
  
  # which variations have average <= threshold?
  tests <- data.frame(sapply(tests,function(x){x$estimate}),sapply(tests,function(x){x$p.value}))
  names(tests)<- c(paste("distance from",threshold,"difference"),"p")
  tests$`Indicator` <-names(msni_varied) 
  tests$difference_smaller_05<-tests$`distance from 0.5 difference` < 0
  tests$p_bonferroni_corrected<-tests$p*nrow(tests)
  tests$dif_significantly_smaller_05<-tests$difference_smaller_05 & tests$p_bonferroni_corrected<0.01
  tests$`average absolute difference from original msni index`<-tests$`distance from 0.5 difference`+threshold
  tests$`p value (Bonferroni corrected)`<-round(tests$p_bonferroni_corrected,5)
  tests$`Difference signifcantly smaller than threshold (at p<=0.01)`<-tests$dif_significantly_smaller_05 %>% as.character %>% recode("TRUE" = "Yes","FALSE" = "No")
  tests %<>% arrange(dif_significantly_smaller_05,desc(`average absolute difference from original msni index`))
  tests$`Difference signifcantly smaller than threshold (at p<=0.01)`<- tests$`Difference signifcantly smaller than threshold (at p<=0.01)` %>% cell_spec(background = ifelse(tests$`Difference signifcantly smaller than threshold (at p<=0.01)`=="Yes","green","red"),color = "white")
  tests %>% select(`Indicator`,`average absolute difference from original msni index`,`p value (Bonferroni corrected)`,`Difference signifcantly smaller than threshold (at p<=0.01)`)
  
}

