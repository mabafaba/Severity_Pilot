msna18_severity_pilot_load_assessment<-function(group){
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




load_assessment<-function(data_csv,
                          loops_csv = c(),
                          questions_csv,
                          choices_csv, 
                          samplingframe_csv,
                          data.stratum.column,
                          sampling.frame.population.column,
                          sampling.frame.stratum.column,
                          default_disaggregation,...){

# read csv files & standardise
data<-read.csv(data_csv,stringsAsFactors = F) %>% standardise_data(data.stratum.column = data.stratum.column)

loops<-NULL
if(length(loops_csv)>0){
  
  loops<-purrr::map(loops_csv,function(filename){
    read.csv(filename,stringsAsFactors = F)    
  })
  names(loops)<-names(loops_csv)
}


questions<-read.csv(questions_csv, stringsAsFactors = F) %>% standardise_questions
choices<-read.csv(choices_csv,stringsAsFactors = F) %>% standardise_choices

samplingframe <- read.csv(samplingframe_csv, stringsAsFactors = F) %>%
    standardise_samplingframe(sampling.frame.stratum.column = sampling.frame.stratum.column,
                            sampling.frame.population.column = sampling.frame.population.column)

if(any(!(data[[data.stratum.column]] %in% samplingframe[[sampling.frame.stratum.column]]))){
  warning("removing records from data that do not match a samplingframe stratum")
  data<-data[data[[data.stratum.column]] %in% samplingframe[[sampling.frame.stratum.column]],,drop = FALSE]
}

# special objects for questionnaire and weighting
questionnaire <- load_questionnaire(data,
                                    questions,
                                    choices)



weighting<-map_to_weighting(samplingframe,
                            data.stratum.column = data.stratum.column,
                            sampling.frame.population.column = sampling.frame.population.column,
                            sampling.frame.stratum.column = sampling.frame.stratum.column,data = data
                           )


return(c(list(data=data,
            questionnaire = questionnaire,
            weighting = weighting,
            default_disaggregation = default_disaggregation,
            loops = loops),
         ...))


}



remove_spaces<-function(x){
  gsub(" ", "", x)
}


standardise_choices<-function(choices){
  choices$name<-remove_spaces(choices$name)
  choices$list_name<-remove_spaces(choices$list_name)
  choices
}

standardise_questions<-function(questions){
  questions$name <- remove_spaces(questions$name)
  questions
}


standardise_samplingframe<-function(sf,sampling.frame.stratum.column, sampling.frame.population.column){
  sf[[sampling.frame.stratum.column]]<-tolower(sf[[sampling.frame.stratum.column]])
  sf[ !(sf[[sampling.frame.population.column]] %in% c(0,"",NA)) , , drop = F]
}

standardise_data<-function(data,data.stratum.column){
  data[[data.stratum.column]]<-tolower(as.character(data[[data.stratum.column]]))
  data
}

remove_non_consent<-function(data,consent_col = "survey_consent", consent_value = "yes"){
  data[data[[consent_col]]=="yes",,drop = FALSE]
}


