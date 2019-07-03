


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













all_combinations<-function(severity){


  # table of variables and associated sector
  vars<-data.frame(varname = names(severity),
                   sector = names(severity) %>% 
                     strsplit("\\.") %>%
                     lapply(function(x){x[2]}) %>% unlist,
                   stringsAsFactors = FALSE)
  vars<-vars[!is.na(vars$varname) & !is.na(vars$sector),]
  combinations<-purrr::map(unique(vars$sector),function(sector){
    sector_varnames<-vars[which(vars$sector==sector),"varname"]
    severity[,sector_varnames] %>%
      lapply(unique) %>% expand.grid   
  })

  names(combinations)<-unique(vars$sector)    
combinations

}

host_assessment
all_combos<-all_combinations(severity = host_assessment$severity) 
library(purrr)


map2(all_combos,paste0(names(all_combos),".csv"),write.csv)
getwd()





all_combos$health
variations<-vary_dataset(refugee_assessment$data,
                         c("soap_in_hh", "formula_received"))


severities<-variations %>% lapply(refugee_severity_bgd_msna18,
                                  refugee_assessment$loops$individuals)


