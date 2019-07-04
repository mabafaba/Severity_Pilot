




#' create variations of a dataset
#' @param variable a name in the dataset. Each unique repsonse in the variable will create a variation in which the whole variable has that one response
#' @return a list of datasets same shape as input but variables varied
vary_dataset<-function(data, variables){
  
  
  vars_and_choices<-purrr::map(variables,
                                function(x,y){
                                  data.frame(var = x, responses = unique(data[[x]]),stringsAsFactors = FALSE)}) %>% 
    do.call(rbind,.) 
  
  
  replace_in_var<-function(var,response){
    variation <- data
    variation[[var]] <- response
    variation
  }
  variations<-  
    purrr::map2(vars_and_choices$var, vars_and_choices$responses,
    replace_in_var
    )  

  names(variations)<-apply(vars_and_choices,1,paste, collapse = c(" - "))
  variations
    
}




variations<-vary_dataset(refugee_assessment$data,
                         c("soap_in_hh", "formula_received"))


severities<-variations %>% lapply(refugee_severity_bgd_msna18,
                                  refugee_assessment$loops$individuals)


