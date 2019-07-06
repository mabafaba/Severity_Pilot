read_all_csvs<-function(folder){
  filenames<-list.files(folder) %>% paste0(folder,"/",.)
  condition_tables<-lapply(filenames,
                           read.csv,
                           stringsAsFactors = FALSE)
  pure_names<-strsplit(filenames,"/") %>% lapply(last) %>% unlist %>% gsub("\\.csv","",.)
  names(condition_tables)<-pure_names
  condition_tables
  
}


#' calcualte subpillars from decision tree table
#' 
#' 
#' @param file the name of the xls file with the choice combinations
#' @param data the dataset/variables for which to compute the subpillars
#' 
subpillar_scores_bgd<-function(list_of_combination_tables,data){





subpillar_scores <- purrr::map(list_of_combination_tables,
           solve_combination_table_bgd,
           values_data = data) 

subpillar_scores <- do.call(data.frame,c(subpillar_scores,stringsAsFactors = FALSE))
names(subpillar_scores)<-names(list_of_combination_tables)
subpillar_scores %<>% as_tibble

subpillar_scores

}










solve_combination_table_bgd<-function(combination_table,values_data){
  
  values<-combination_table %>% select(starts_with("si."))
  target<-combination_table %>% select(starts_with("JIAF."))
  solved<-solve_combination_table(values, target, values_data) %>% unlist %>%  as.character %>% as.numeric %>% tibble(solution = .)
  
  
}













solve_combination_table<-function(values, target, values_data){

  values<- values[,!(names(values) %in% c("", NA, " ")),drop = FALSE]
if(!all(names(values) %in% names(values_data))){
  stop(paste(
    "all column names in the combinations table must appear in the data. Missing:",
    paste(names(values)[!(names(values)%in%names(values_data))],collapse = "; ")))
}
  lookup_table_combo <- pasterows(values,"___")
  data_values_combo <- pasterows(values_data[,names(values),drop = FALSE],"___")
  lookup_table_index <- match(data_values_combo,lookup_table_combo)
  
  target[lookup_table_index,]

}





pasterows<-function(x,sep=" "){
  do.call(paste,c(x,sep = sep))  
}




