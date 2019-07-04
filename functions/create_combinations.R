


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

create_empty_combination_tables<-function(target_dir = "./output/"){
    
    
    host_assessment <- load_assessment(data_csv = "./input/HCMSNA_csv/HC_HH_Data.csv",
                                       loops_csv = c(individuals = "./input/HCMSNA_csv/HC_Indiv_Data.csv"),
                                       questions_csv = "./input/HCMSNA_csv/questions_hc.csv" ,
                                       choices_csv = "./input/HCMSNA_csv/choices_hc.csv",
                                       samplingframe_csv = "./input/HCMSNA_csv/samplingframe.csv",
                                       data.stratum.column = "union_name",
                                       sampling.frame.population.column = "NbHH",
                                       sampling.frame.stratum.column = "union_id",
                                       
                                       default_disaggregation = "union_name",
                                       asdf = "xasdf")
    
    
    refugee_assessment <- load_assessment(data_csv = "./input/RefugeeMSNA_csv/Refugee_MSNA_HH.csv",
                                          loops_csv = c(individuals = "./input/RefugeeMSNA_csv/Refugee_MSNA_Indiv_Data.csv"),
                                          questions_csv = "./input/RefugeeMSNA_csv/questions_refugee.csv" ,
                                          choices_csv = "./input/RefugeeMSNA_csv/choices_refugee.csv",
                                          samplingframe_csv = "./input/RefugeeMSNA_csv/samplingframe_refugee.csv",
                                          data.stratum.column = "camp_location",
                                          sampling.frame.population.column = "Total.Families",
                                          sampling.frame.stratum.column = "Camps",
                                          
                                          default_disaggregation = "camp_location")
    
    
    
    
    host_assessment$severity <- host_severity_bgd_msna18(hh = host_assessment$data,
                                                         host_assessment$loops$individuals)
    
    
    
    refugee_assessment$severity <- refugee_severity_bgd_msna18(hh  = refugee_assessment$data,
                                                               ind = refugee_assessment$loops$individuals)
    
    
    
    dir.create(paste0(target.dir, "/", "host"),recursive = T)
    dir.create(paste0(target.dir, "/", "refugee"),recursive = T)
    unlink(list.files(target_dir),recursive = T)
    host_combos<-all_combinations(host_assessment$sevserity)
    ref_combos<-all_combinations(refugee_assessment$severity)
    map2(host_combos,paste0(paste0(target.dir, "/", "host/"),names(host_combos),".csv"),write.csv, row.names = FALSE)
    map2(ref_combos,paste0(paste0(target.dir, "/", "refugee/"),names(ref_combos),".csv"),write.csv, row.names = FALSE)
    
    
    

}





