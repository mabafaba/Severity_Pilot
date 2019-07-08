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
                                  shp_dir = "input_public/01_shp",
                                  layer_name = "BGD_Teknaf_Ukhia_Unions")
    
    
    assessment$shp$match_id <- str_standardize_cxb_camps(assessment$shp$adm4_en)
    assessment$shp$region <- assessment$shp$adm3_en
    assessment$data<-host_refugee_add_standard_regions_to_data(assessment$data, location_column = 'union_name')
    

  }else if(group=="refugee"){
    assessment <- load_assessment(data_csv = "./input/RefugeeMSNA_csv/Refugee_MSNA_HH.csv",
                                  loops_csv = c(individuals = "./input/RefugeeMSNA_csv/Refugee_MSNA_Indiv_Data.csv"),
                                  questions_csv = "./input/RefugeeMSNA_csv/questions_refugee.csv" ,
                                  choices_csv = "./input/RefugeeMSNA_csv/choices_refugee.csv",
                                  samplingframe_csv = "./input/RefugeeMSNA_csv/samplingframe_refugee.csv",
                                  data.stratum.column = "camp_location",
                                  sampling.frame.population.column = "Total.Families",
                                  sampling.frame.stratum.column = "Camps",
                                  default_disaggregation = "camp_location",
                                  shp_dir = "input_public/01_shp",
                                  layer_name = "190310_Outline_Rohingya_Refugee_Camp_A1")
    
    
    assessment$shp$match_id <- str_standardize_cxb_camps(assessment$shp$New_Camp_N)
    assessment$shp@data <- bgd_refugee_add_standard_regions_to_data(assessment$shp@data,location_column = 'New_Camp_N')
    
    assessment$data$match_id <- str_standardize_cxb_camps(assessment$data$camp_location)
    
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
                          default_disaggregation,
                          shp_dir = NULL,
                          layer_name = NULL,...){

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



if(!is.null(shp_dir) & !is.null(layer_name)){
  shp<-readOGR(dsn=shp_dir, layer = layer_name, stringsAsFactors = FALSE)
}




return(c(list(data=data,
            questionnaire = questionnaire,
            weighting = weighting,
            default_disaggregation = default_disaggregation,
            loops = loops,
            shp = shp),
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









bgd_refugee_add_standard_regions_to_data<-function(data,location_column){
  
  regions<-list(kbc = c("camp 13", "camp 14", "camp 10",  "camp 6", "camp 18", 
                        "camp 20", "camp 1e",  "camp 17", "camp 9", 
                        "camp 8w", "camp 1w", "camp 15", "camp 5" , "camp 3", 
                        "camp 16", "camp 2w", "camp 20 extension", "camp 11", "camp 4", 
                        "camp 19",  "camp 7", "camp 4 extension","kutupalong rc",
                        "camp 8e", "camp 2e", "camp 12"),
                iso = c("camp 21", "camp 22","camp 23","choukhali"),
                st = c("camp 24","camp 25","camp 26","camp 27", "nayapara rc"))
  
  
  camp_name<-str_standardize_cxb_camps(data[,location_column])
  
  attach(regions)
  data <- data %>% 
    mutate(
      region = ifelse(
      camp_name %in% kbc,
      "Kutapalong Megacamp",
      ifelse(camp_name %in% iso, "Detached Camps", "Southern Teknaf")
    ))

  detach(regions)
  data
}



host_refugee_add_standard_regions_to_data<-function(data,location_column){
  
  data$match_id<-str_standardize(data[,location_column])
  data
  
  
}

assessment_data_as_sf<-function(data,assessment, by.data = 'match_id', by.shp = 'match_id'){
  shapes<-sp::merge(assessment$shp, data, by.x= by.shp, by.y = by.data, all.x=TRUE, all.y=FALSE)
  sfdf <- st_as_sf(shapes)
  
}

