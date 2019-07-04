
bgd_severity_plot_load_assessment<-function(group){
if(group=="host"){
  
  assessment <- load_assessment(data_csv = "./input/HCMSNA_csv/HC_HH_Data.csv",
                                loops_csv = c(individuals = "./input/HCMSNA_csv/HC_Indiv_Data.csv"),
                                questions_csv = "./input/HCMSNA_csv/questions_hc.csv" ,
                                choices_csv = "./input/HCMSNA_csv/choices_hc.csv",
                                samplingframe_csv = "./input/HCMSNA_csv/samplingframe.csv",
                                data.stratum.column = "union_name",
                                sampling.frame.population.column = "NbHH",
                                sampling.frame.stratum.column = "union_id",
                                
                                default_disaggregation = "union_name",
                                asdf = "xasdf")
  
  
}else if(group=="refugee"){
  assessment <- load_assessment(data_csv = "./input/RefugeeMSNA_csv/Refugee_MSNA_HH.csv",
                                loops_csv = c(individuals = "./input/RefugeeMSNA_csv/Refugee_MSNA_Indiv_Data.csv"),
                                questions_csv = "./input/RefugeeMSNA_csv/questions_refugee.csv" ,
                                choices_csv = "./input/RefugeeMSNA_csv/choices_refugee.csv",
                                samplingframe_csv = "./input/RefugeeMSNA_csv/samplingframe_refugee.csv",
                                data.stratum.column = "camp_location",
                                sampling.frame.population.column = "Total.Families",
                                sampling.frame.stratum.column = "Camps",
                                default_disaggregation = "camp_location")
  
  
  
}else{
  stop("'group' parameter at top of Rmd must be 'host' or 'refugee")
}
  return(assessment)
}