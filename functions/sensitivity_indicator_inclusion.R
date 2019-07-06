


msni_variations<-function(combination_tables,severity,variation_function){

  varied_combination_tables<-variation_function(combination_tables)
  
  varied_scores<-purrr::map(varied_combination_tables,
                            subpillar_scores_bgd,
                            severity)
  
  
  varied_scores<-purrr::map(varied_scores,function(x){
    x$impact<-4
    x
  })
  
  varied_msni<-varied_scores %>% purrr::map(function(subpillars){
    msni19::msni(education_lsg = subpillars$edu,
                 fsl_lsg = subpillars$fsl,
                 health_lsg = subpillars$health,
                 protection_lsg = subpillars$protection,
                 shelter_lsg = subpillars$shelter,
                 wash_lsg = subpillars$wash,
                 capacity_gaps = subpillars$capacity,impact = subpillars$impact)
  }) %>% do.call(cbind,.) %>% as_tibble 
  
}





















