compare_each <-
  function(data,
           dependent.variable,
           dependent.variable.type,
           independent.variable,
           weighting = NULL,
           cluster_variable_name = NULL) {
    unique_independent.var.values <- unique(data[[independent.variable]])
    
    
    lapply(
      unique_independent.var.values,
      compare_one_choices_against_all_other,
      data = data,
      dependent.variable = dependent.variable,
      dependent.variable.type = dependent.variable.type,
      independent.variable = independent.variable,
      weighting = weighting,
      cluster_variable_name = cluster_variable_name
    )
    
    
    
    
    
  }




compare_one_choices_against_all_other <- function(choice,data,
                                                  dependent.variable,
                                                  dependent.variable.type,
                                                  independent.variable,
                                                  weighting = NULL,
                                                  cluster_variable_name = NULL,
                                                  questionnaire = NULL) {
  
  data[["CHOICE_VARIABLE"]] <- data[[independent.variable]] == choice
  data[["CHOICE_VARIABLE_VALUES"]]<-  data[["CHOICE_VARIABLE"]]
  data[["CHOICE_VARIABLE_VALUES"]][data[["CHOICE_VARIABLE"]]] <- choice
  data[["CHOICE_VARIABLE_VALUES"]][!data[["CHOICE_VARIABLE"]]] <- paste0("All Other Responses")
  
  case <- map_to_case(
    hypothesis.type = "group_difference",
    dependent.var.type = dependent.variable.type,
    independent.var.type = "categorical"
  )
  
  result <- map_to_result(
    data,
    dependent.var = dependent.variable,
    independent.var = "CHOICE_VARIABLE_VALUES",
    case = case,
    cluster.variable.name = cluster_variable_name,
    weighting = weighting
  )
  
  result<-result %>%
    (function(x){
      x$summary.statistic$p.value <- x$hypothesis.test$result$p.value
      x$summary.statistic$independent.var<-independent.variable
      x
    }) 
  
  result$summary.statistic$other.numbers<-result$summary.statistic$numbers[result$summary.statistic$independent.var.value=="All Other Responses"]
  result$summary.statistic$other.min<-result$summary.statistic$min[result$summary.statistic$independent.var.value=="All Other Responses"]
  result$summary.statistic$other.max<-result$summary.statistic$max[result$summary.statistic$independent.var.value=="All Other Responses"]
  result$summary.statistic<-result$summary.statistic[result$summary.statistic$independent.var.value!="All Other Responses",,drop = FALSE]
  # result$summary.statistic %>% gather(,,min,max,numbers,p.value) %>%
  # mutate(what = paste(independent.var.value,"-",key)) %>% select(-key,-independent.var.value) %>%  
  # spread(what,value)
  
  # spread(result$summary.statistic, key = independent.var.value,value = numbers,sep="-") 
  result
}
