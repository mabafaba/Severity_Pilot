


vary_dataset<-function(data, variables, responses){
  
  vars_and_choices<-purrr::map2(variables,responses,function(x,y){data.frame(var = x, response = y)})
  
  
  vars_and_choices %>%  
    purrr::map(function(var_and_choices){
      variation <- data
      variation[[var]] <- 
    })
  
    
}


severities<-variations %>% lapply(refugee_severity_bgd_msna18,refugee_assessment$loops$individuals)

fsl<-severities %>% lapply(function(x){x$si.fsl}) %>% as.data.frame


names(fsl)<-c("yes","no")


ggplot(fsl)+geom_point(aes(x=yes,y=no))+
  geom_point(refugee_assessment$severity$si.fsl,aes())




refugee_assessment$severity <- refugee_severity_bgd_msna18(hh  = refugee_assessment$data,
                                                           ind = refugee_assessment$loops$individuals)
