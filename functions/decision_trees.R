#' calcualte subpillars from decision tree table
#' 
#' 
#' @param file the name of the xls file with the choice combinations
#' @param subpillars character vector of subpillars; should match sheet names in `file`
#' @param data the dataset/variables for which to compute the subpillars
#' 
subpillar_scores_from_xls_bgd<-function(file,subpillars,data){
  
condition_tables<-lapply(subpillars,
                         xlsx::read.xlsx2,
                         file =  file,
                         stringsAsFactors = FALSE)


subpillar_scores <- purrr::map(condition_tables,
           solve_combination_table_bgd,
           values_data = data) 

subpillar_scores <- do.call(data.frame,c(subpillar_scores,stringsAsFactors = FALSE))
names(subpillar_scores)<-subpillars
subpillar_scores %<>% as_tibble

subpillar_scores

}










solve_combination_table_bgd<-function(combination_table,values_data){
  
  values<-combination_table %>% select(starts_with("si."))
  target<-combination_table %>% select(starts_with("JIAF."))
  solve_combination_table(values, target, values_data) %>% as.character %>% as.numeric
  
  
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




