

hc_severity<-function(data,questionnaire){
  
  runif(nrow(data),min = 1,max = 4) %>% round
  
}


ref_severity<-function(data,questionnaire){
  
  runif(nrow(data),min = 1,max = 4) %>% round
  
}


# 
# 
# load_assessment<-function(data.csv,
#                           question.csv,
#                           choices.csv,
#                           samplingframe.csv,
# ){
#   
# }


