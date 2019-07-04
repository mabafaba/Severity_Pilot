

#' Calculate severity based on decision tree specifically for BGD 2018 refugee MSNA
#'  @param hh the houshold level dataset (with water container information merged)
#'  @param ind the indvidual level data
refugee_severity_bgd_msna18<-function(hh,ind){
  hhy<-hh
  df<-merge(hhy,ind,by.x="instance_name", by.y= "parent_instance_name", all.x=TRUE) 
  
  indiv_to_hh <- df %>% 
    mutate(
      worked= ifelse(individual_worked== "yes",1,0),
      worked_over18= ifelse(individual_age>17 & individual_worked=="yes",1,0),
      worked_under18= ifelse(individual_age<18 & individual_worked=="yes",1,0),
      dia= ifelse(is.na(under5_diarrhea), "no", under5_diarrhea),
      dia_trt_ors_home= ifelse(is.na(df$under5_ors.yes_home),"NOT_RELEVANT", df$under5_ors.yes_home),
      dia_trt_ors_healhcare= ifelse(is.na(df$under5_ors.yes_healthcare),"NOT_RELEVANT", df$under5_ors.yes_healthcare),
      dia_no_ors= ifelse(dia=="yes" &  (dia_trt_ors_home=="FALSE" | dia_trt_ors_healhcare=="FALSE"),1,0),
      dia_yes_ors= ifelse(dia=="yes" &  (dia_trt_ors_home=="TRUE" | dia_trt_ors_healhcare=="TRUE"),1,0),
      child_lab_none=ifelse(is.na(childlabour_worstforms.none),TRUE,FALSE)
    ) %>% 
    group_by(instance_name) %>% 
    summarise(
      
      #COUNT-IF ANSWER =...  IF FAMILY HAS >0 KIDS IN SCHOOL THEY 
      
      si.edu.learning_space_attendance=ifelse(sum(learning_space_attendance=="no",na.rm=TRUE)>=3,2, ifelse(sum(learning_space_attendance=="no",na.rm=TRUE)>=1,1,0)),
      si.edu.non_religious_learning=ifelse(sum(religious_space_attendance=="no",na.rm=TRUE)>=3,2, ifelse(sum(religious_space_attendance=="no",na.rm=TRUE)>=1,1,0)),
      si.nfi.mosquito_net=ifelse(sum(under5_mosquito_net=="no", na.rm=TRUE)>1,2 ,ifelse(sum(under5_mosquito_net=="yes", na.rm=TRUE)>0,1,0)),
      si.health.illness.serious=ifelse(sum(individual_illness=="yes", na.rm=TRUE)>1,2 ,ifelse(sum(individual_illness=="yes", na.rm=TRUE)>0,1,0)),
      si.health.dia_ors=ifelse(sum(dia_no_ors,na.rm=TRUE)>0,2,ifelse(sum(dia_yes_ors,na.rm=TRUE)==sum(dia=="yes",na.rm=TRUE),1,0)),
      worked_adults= ifelse(sum(worked_over18,na.rm=TRUE)>0,1,0),
      worked_kids= ifelse(sum(worked_under18,na.rm=TRUE)>0,1,0 ),
      food_recieved1= unique(food_received),
      no_one_worked= ifelse(sum(worked,na.rm=TRUE)<1,1,0),
      si.capacity_gap.child_lab_bad= ifelse(sum(child_lab_none==FALSE,na.rm=TRUE)>0,2,0)
      
    ) %>% 
    mutate(
      si.fsl=ifelse(worked_kids==0 & food_recieved1=="yes",0,
                    ifelse(worked_kids==1 & food_recieved1=="yes",1,
                           ifelse(no_one_worked==0 & food_recieved1=="no",2,
                                  ifelse(no_one_worked==1 & food_recieved1=="no",3,"YOU MISSED SOMETHING")))
      )) %>% 
    select(instance_name, starts_with("si."))
  #########################
  
  unsafe_location_cols<-c("unsafe_men.shelter", "unsafe_men.latrines", "unsafe_men.market", 
                          "unsafe_men.health_centre", "unsafe_men.water_points", "unsafe_men.bathing_areas", 
                          "unsafe_men.learning_spaces", "unsafe_men.distribution_points", 
                          "unsafe_men.firewood_collection", "unsafe_men.home", 
                          "unsafe_men.other", "unsafe_women.shelter", 
                          "unsafe_women.latrines", "unsafe_women.market", "unsafe_women.health_centre", 
                          "unsafe_women.water_points", "unsafe_women.bathing_areas", "unsafe_women.learning_spaces", 
                          "unsafe_women.distribution_points", "unsafe_women.firewood_collection", 
                          "unsafe_women.home", 
                          "unsafe_women.other", "unsafe_boys.shelter", "unsafe_boys.latrines", 
                          "unsafe_boys.market", "unsafe_boys.health_centre", "unsafe_boys.water_points", 
                          "unsafe_boys.bathing_areas", "unsafe_boys.learning_spaces", "unsafe_boys.distribution_points", 
                          "unsafe_boys.firewood_collection", "unsafe_boys.home",  
                          "unsafe_boys.other", "unsafe_girls.shelter", 
                          "unsafe_girls.latrines", "unsafe_girls.market", "unsafe_girls.health_centre", 
                          "unsafe_girls.water_points", "unsafe_girls.bathing_areas", "unsafe_girls.learning_spaces", 
                          "unsafe_girls.distribution_points", "unsafe_girls.firewood_collection", 
                          "unsafe_girls.home", 
                          "unsafe_girls.other")
  
  essential_nfis<-c("hh_nfis.solar_lamp", "hh_nfis.portable_lamp", "hh_nfis.kitchen_set", 
                    "hh_nfis.floor_mat", "hh_nfis.cooking_stove","hh_nfis.fuel")
  protected_water_sources<- c("tube", "pip", "wat_tnk", "crt", "pwell", "bwat", "tnktr")
  
  hh_level_indicators<-hhy%>% 
    mutate(
      si.protection.lighting=ifelse(lighting_availability=="yes", 2,0),
      si.protection.unsafe=ifelse(rowSums(.[unsafe_location_cols],na.rm=TRUE)>8,2,
                               ifelse(rowSums(.[unsafe_location_cols],na.rm = TRUE)>4,1,0)),
      si.nfi.shelt_damage= ifelse(shelter_damage.roof_destroyed=="yes"| shelter_damage.wall_destroyed=="yes",2,
                                  ifelse((shelter_damage.roof_damaged=="yes"|shelter_damage.wall_damaged=="yes")& (shelter_damage.roof_destroyed=="no"|
                                                                                                                     shelter_damage.wall_destroyed=="no"),1,0)),
      si.nfi.essential=ifelse(rowSums(.[essential_nfis],na.rm=TRUE)>=3,2,
                              ifelse(rowSums(.[essential_nfis],na.rm=TRUE)>=1,1,0)),
      si.wash.defec=ifelse(defecate_where.opn_defecation==TRUE,2,
                           ifelse(defecate_where.communal_latrine==TRUE|
                                    defecate_where.plastic_bag==TRUE|
                                    defecate_where.bucket_toilet==TRUE,1,0)),
      dom_water_pppd= vol_total_water/hh_size,
      si.wash.drnk_dom_water=ifelse(dom_water_pppd<3,2,
                                    ifelse(dom_water_pppd<15,1,0)),
      si.wash.garbage_disp= ifelse(garbage_disposal %in% c( "burn", "bury", "into_stream"),2,0),
      si.wash.improved_water= ifelse(drnk_wat %in% protected_water_sources,0,2),
      marriage_burden_reduce= ifelse(is.na(child_marriage_reasons.reduce_burden),FALSE,child_marriage_reasons.reduce_burden),
      si.capacity_gap_child_marriage_reduce_burden= ifelse(marriage_burden_reduce==TRUE,2, 0),
      debt_amount_recode=ifelse(is.na(debt_amount),0,debt_amount),
      si.capacity_gaps_debt= ifelse(debt_amount_recode>=25000,2,ifelse(debt_amount_recode>=10000,1,ifelse(is.na(debt_amount_recode),0,0))),
      health.preg= ifelse(is.na(anc_group.pregnant_number),0,anc_group.pregnant_number),
      health.anc=ifelse(is.na(anc_group.pregnant_anc),0,anc_group.pregnant_anc),
      si.health.preg= ifelse(health.preg<1,0,ifelse(health.preg>health.anc,2,0))
    ) %>% select(instance_name, starts_with("si."))
  
  
  dl<-list(indiv_to_hh,hh_level_indicators)
    
  hh_indicators_combined<-Reduce(function(x, y) merge(x, y, all=TRUE), dl)
  hh_indicators_combined[,-1]<-lapply(hh_indicators_combined[,-1],as.numeric) %>% as_tibble
  
  
  
  
  

  
  subpillar_scores <- subpillar_scores_from_xls(  file =  "./input/decision_tree_refugees.xlsx",
                              subpillars = subpillars<-c(
                                "edu",
                                "nfi",
                                "fsl",
                                "health",
                                "protection"
                                # ,
                                # "ios",
                                # "wash"
                              ),
                              data = hh_indicators_combined)
  
  return(c(subpillar_scores,hh_indicators_combined) %>% as_tibble)
  
}
