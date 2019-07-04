




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






#' create tables of all existing result combinations of sub indicators
all_combinations<-function(severity){


  # table of variables and associated sector
  vars<-data.frame(varname = names(severity),
                   sector = names(severity) %>% 
                     strsplit("\\.") %>%
                     lapply(function(x){x[2]}) %>% unlist,
                   stringsAsFactors = FALSE)
  vars<-vars[!is.na(vars$varname) & !is.na(vars$sector),]
  sector<-"health"
  combinations<-purrr::map(unique(vars$sector),function(sector){
    sector_varnames<-vars[which(vars$sector==sector),"varname"]
    severity[,sector_varnames,drop = F] %>%
      lapply(unique) %>% lapply(function(x){x[!is.na(x)]}) %>% expand.grid   
  })

  names(combinations)<-unique(vars$sector)    
combinations

}



# dir.create("./output/host",recursive = T)
# dir.create("./output/refugee",recursive = T)
# unlink(list.files("./output"),recursive = T)
# host_combos<-all_combinations(host_assessment$severity)
# ref_combos<-all_combinations(refugee_assessment$severity)
# map2(host_combos,paste0("./output/host/",names(host_combos),".csv"),write.csv, row.names = FALSE)
# map2(ref_combos,paste0("./output/refugee/",names(ref_combos),".csv"),write.csv, row.names = FALSE)


variations<-vary_dataset(refugee_assessment$data,
                         c("soap_in_hh", "formula_received"))


severities<-variations %>% lapply(refugee_severity_bgd_msna18,
                                  refugee_assessment$loops$individuals)


