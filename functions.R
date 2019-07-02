

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



weighted_ecdf<-function(x,weights){
  df<-tibble(x=x,w=weights)
  df <- df[order(df$x), ]  # Won't change anything since it was created sorted
  cum.pct <- cumsum(x * weights) / sum(x * weights)
  df$cum.pct <- with(df, cumsum(x * w) / sum(x * w))
  df
}



load_assessment<-function(data_csv,
                          questions_csv,
                          choices_csv, 
                          samplingframe_csv,
                          data.stratum.column,
                          sampling.frame.population.column,
                          sampling.frame.stratum.column){

data<-read.csv(data_csv,stringsAsFactors = F)
questions<-read.csv(questions_csv, stringsAsFactors = F) %>% standardise_questions
choices<-read.csv(choices_csv,stringsAsFactors = F) %>% standardise_choices
samplingframe <- read.csv(samplingframe_csv, stringsAsFactors = F)

questionnaire <- load_questionnaire(data,
                                    questions,
                                    choices)

weighting<-map_to_weighting(samplingframe,
                            data.stratum.column = data.stratum.column,
                            sampling.frame.population.column = sampling.frame.population.column,
                            sampling.frame.stratum.column = sampling.frame.stratum.column,data = data)


return(list(data=data,questionnaire = questionnaire,weighting = weighting))


}
