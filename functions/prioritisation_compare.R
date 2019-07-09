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






kable_comparison_result<-function(results, independent.variable.title){
  
  results<-comparisons
  comparisons_df <- comparisons  %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)
  comparisons_df$p.value.corrected<-comparisons_df$p.value * nrow(comparisons_df)
  
  comparisons_df<-comparisons_df %>%
    select(independent.var.value,numbers,other.numbers,p.value.corrected) %>% 
    mutate("Difference" = other.numbers - numbers) %>% 
    arrange(desc(other.numbers - numbers)) %>%
    mutate("Significant (at p < 0.01)" = ifelse(p.value.corrected<=0.01,"Yes","No"))
  
  comparisons_df$`Significant (at p < 0.01)`  <-  comparisons_df$`Significant (at p < 0.01)` %>% cell_spec(background = ifelse(comparisons_df$`Significant (at p < 0.01)`=="Yes","green","red"),color = 'white')
  
  comparisons_df$independent.var.value<-questionnaire$question_get_choice_labels(comparisons_df$independent.var.value,variable.name = default_disaggregation)
  comparisons_df<-comparisons_df %>% rename(`Obverved` = numbers, "Observed (all other)" = other.numbers, "p Value (Bonferroni corrected)" = p.value.corrected)
  names(comparisons_df)[which(names(comparisons_df)=="independent.var.value")]<-independent.variable.title
  
  comparisons_df %>% kable(escape = FALSE) %>% kable_styling()
  
  
}



kable_comparison_result_categorical<-function(results,independent.variable.title){

  comparisons<-results
  
  
  reformat_to_difference_rows<-function(sumstat){
    sumstat<-sumstat %>% 
      select(dependent.var.value,independent.var.value,numbers,other.numbers,p.value) %>%
      mutate("Difference" = numbers - other.numbers) %>% 
      gather(,"Difference",Difference) %>%
      arrange(as.numeric(as.character(dependent.var.value))) %>% select(independent.var.value,Difference, dependent.var.value,p.value) %>% 
      spread(key = dependent.var.value,value = Difference)
    
    colnames(sumstat)<-c(independent.variable.title,"p Value",paste("percentage points difference to others",1:4))
    sumstat
  }

comparisons_df <- comparisons  %>% lapply(function(x){x$summary.statistic}) %>% lapply(reformat_to_difference_rows) %>% do.call(rbind,.)
comparisons_df$`P Value (Bonferroni corrected)`<-comparisons_df$`p Value` * nrow(comparisons_df)
comparisons_df[[independent.variable.title]]<-questionnaire$question_get_choice_labels(comparisons_df[[independent.variable.title]],default_disaggregation)


comparisons_df <- comparisons_df %>%
  mutate("Significant (at p < 0.01)" = ifelse(comparisons_df$`P Value (Bonferroni corrected)`<=0.01,"Yes","No"))

comparisons_df$`Significant (at p < 0.01)`  <-  comparisons_df$`Significant (at p < 0.01)` %>% cell_spec(background = ifelse(comparisons_df$`Significant (at p < 0.01)`=="Yes","green","red"),color = 'white')
comparisons_df<-comparisons_df[1,,drop = FALSE]
comparisons_df %>% kable(escape = FALSE) %>% kable_styling()


}


