host_severity_bgd_msna18<-function(hh,ind){
  hhy<-hh
  df<-merge(hhy,ind,by.x="instance_name", by.y= "parent_instance_name", all.x=TRUE) 
  
  sum.na.rm<-purrr::partial(sum,na.rm = T)
  Individual_to_HH<-df %>% 
    mutate(
      working_anyone= individual_work=="yes",
      working_child= ifelse(individual_work=="yes" & individual_age<18,1,0),
      working_adult=ifelse(individual_work=="yes" & individual_age>=18,1,0),
      not_in_formal_school =  formal_learn_space=="none",
      not_in_nonformal_school = nonformal_learn_space=="none",
      not_in_any_school =  not_in_formal_school & not_in_nonformal_school,
      
    ) %>% 
    
    group_by(instance_name) %>% # count recoded individuals 
    
    summarise(
      num_not_in_formal_school = sum.na.rm(not_in_formal_school),
      num_not_in_nonformal_school = sum.na.rm(not_in_nonformal_school),
      num_not_in_any_school = sum.na.rm(not_in_any_school),
      num_working_adult= sum.na.rm(working_adult),
      num_working_child=sum.na.rm(working_child),
      num_working_anyone=sum.na.rm(working_anyone),
      
    ) %>% 
    mutate(
      
      #COUNT-IF ANSWER =...  IF FAMILY HAS >0 KIDS IN SCHOOL THEY 
      si.edu.formal_learn_space = ifelse(num_not_in_formal_school>=3,2, ifelse(num_not_in_formal_school>=1,1,0)),
      si.edu.non_formal_learn_space=ifelse(num_not_in_nonformal_school>=3,2, ifelse(num_not_in_nonformal_school>=1,1,0)),
      
      #IF ANY ADULT WORKED THEY GET S0, IF  ONLY CHILD WORKED S1, IF NO ONE WORKED S2 
      si.fsl.individual_worked= ifelse(num_working_adult>0,0,
                                       ifelse(num_working_child>0&num_working_child==num_working_adult,1,
                                              ifelse(num_working_anyone<1,0,NA)))) %>% 
    select(instance_name, starts_with("si."))
  
  
  
  more_individual_to_HH<-df %>% 
    mutate(
      born1=ifelse(is.na(birth_place), "no_baby", birth_place),
      no_dia=ifelse(is.na(child_diarrhea) | child_diarrhea=="no",1,0),yes_dia=ifelse(no_dia==0,1,0),
      treated_rec=ifelse( is.na(diarrhea_treatment), "not_applicable",diarrhea_treatment),
      treated_dia= ifelse(treated_rec!="none",1,0),
      yes_dia_with_treat= ifelse( yes_dia==1 & treated_dia==1,1,0)
    ) %>% 
    
    group_by(instance_name) %>% 
    summarise(
      health_mosq_net=ifelse(mean(mosquito_net=="no",na.rm=TRUE)==1,2,
                             ifelse(mean(mosquito_net=="no", na.rm=TRUE)==0,0,1)),
      si.health.mosq=ifelse(is.na(health_mosq_net),0,health_mosq_net),
      si.health.indiv_illness=ifelse(sum(individual_illness=="yes",na.rm=TRUE)>0,1,
                                     ifelse(sum(individual_illness=="yes",na.rm=TRUE)>1,2,0)),
      si.protection.missing_child=ifelse(sum(missing_child=="yes",na.rm=TRUE)>0,2,
                                         ifelse(is.na(sum(missing_child=="yes")),0,0)),
      si.health.birthplace=ifelse(sum(born1=="home",na.rm=TRUE)>0,2,1),
      si.health.dia=ifelse(mean(no_dia,na.rm=TRUE)==1, 0,ifelse(sum(yes_dia_with_treat,na.rm=TRUE)==sum(yes_dia,na.rm=TRUE),1,2))
      
    ) %>% select(instance_name,starts_with("si.")) 
  
  # STRAIGHT OUTTA HOUSEHOLD
  unsustainable_food_sources<-c("gifts_relatives_friends", 
                                "work_barter_food", "zakat", "begging_food", "aid_govt_ngo_wfp")
  unsustainable_income_sources<-c("main_income.remittances",
                                  "main_income.food",
                                  "main_income.assistance_relative_friends",
                                  "main_income.begging",
                                  "main_income.none",
                                  "main_income.other_cash_assistance",
                                  "main_income.zakat",
                                  "main_income.savings")
  
  sustainable_income_sources<-c("main_income.non_agricultural", "main_income.agricultural", 
                                "main_income.domestic", "main_income.petty_trade", "main_income.small_business", 
                                "main_income.large_business", "main_income.skilled_labour", "main_income.fishing", 
                                "main_income.handicrafts", "main_income.agricultural_production_sale", 
                                "main_income.livestock_rearing" , "main_income.sale_assistance", 
                                "main_income.gather_sell_firewood" )
  essential_nfis<-c("non_food_items.solar_lamp", "non_food_items.portable_torch", 
                    "non_food_items.kitchen_set", "non_food_items.floor_mat", "non_food_items.cooking_stove","non_food_items.fuel")
  
  hh_level<-hhy %>% 
    mutate_at(.var= c("boy_prim_edu_barrier",
                      "girl_prim_edu_barrier",
                      "boy_second_edu_barrier",
                      "girl_second_edu_barrier"),
              .funs = funs(ifelse(.== "no"| is.na(.),0,1))) %>% 
    mutate(
      si.fsl.fcs= ifelse(FCS>42,0, ifelse(FCS>28 & FCS<=42,1,ifelse(FCS<=28,2,NA))),
      si.fsl.unsustainable_food= ifelse(main_source_food %in% unsustainable_food_sources,2,0),
      sum_unsut_income= rowSums(.[unsustainable_income_sources], na.rm=TRUE),
      sum_sust_income= rowSums(.[sustainable_income_sources], na.rm=TRUE),
      si.fsl.main_income=ifelse(sum_sust_income==0 & sum_unsut_income>0,2,ifelse(sum_sust_income>0 &sum_unsut_income>0,1,
                                                                                 ifelse(sum_sust_income>0& sum_unsut_income==0,0,NA))),
      food_exp_ratio=spend_food/TotalExpenditure,
      si.fsl.food_exp= ifelse(food_exp_ratio>0.74, 2, ifelse(food_exp_ratio>0.5,1,ifelse(food_exp_ratio<=0.5,0,NA))),
      si.protection.feel_safe= ifelse(hh_safe=="yes",0,
                                      ifelse(hh_safe=="no",2,NA)),
      nfi_expenditure= spend_clothing + spend_fuel+ spend_hh_items+ spend_hygiene,
      nfi_to_total= nfi_expenditure/TotalExpenditure,
      si.nfi.exp=ifelse(nfi_to_total>=0.25,2,ifelse(nfi_to_total>=0.15,1,0)),
      shelter_expenditure= spend_fix_shelter + spend_rent,
      shelter_to_total=shelter_expenditure/TotalExpenditure,
      si.shelt.exp=ifelse(shelter_to_total>=0.2, 2, ifelse(shelter_to_total<0.2,1,0)),
      si.shelt.topo=ifelse(ShelterType=="Jhuprie",2,
                           ifelse(ShelterType=="Kutcha",1,
                                  ifelse(ShelterType %in% c("Pucca", "SemiPucca"),1,NA))),
      si.nfi.essential= ifelse(rowSums(.[essential_nfis])>1,2,
                               ifelse(rowSums(.[essential_nfis])==1,1,
                                      ifelse(rowSums(.[essential_nfis])<1,0,NA))),
      si.wash.improved_water_access = ifelse(is.na(improved_water_access),2,
                                             ifelse(improved_water_access=="intermittently_unpredictable",1,
                                                    ifelse(improved_water_access %in% c("always_year_round","intermittently_predictable"),0,NA))),
      si.wash.defecation= ifelse(hh_defecation== "open_defecation",2,ifelse( hh_defecation=="communal_latrine",1,0)),
      si.wash.enough_water= ifelse(enough_water=="yes",0,2),
      si.wash.soap= ifelse(soap_handwash %in% c("yes_saw_soap", "yes_didnt_see_soap"),0,2),
      si.wash.visible_feces=ifelse(visible_faeces=="yes",2,0),
      si.ios.preg=ifelse(pregnant_women==0| is.na(pregnant_women),0,2),
      si.ios.health_worker= ifelse(comm_health_worker=="yes",1, ifelse(comm_health_worker %in% c("no","dont_know"),2,NA)),
      si.ios.feedback=ifelse(provide_feedback.dont_know==TRUE,2,0),
      si.ios.edu= ifelse(is.na(edu_aid_material)|edu_aid_material=="yes",1,2),
      edu_barrier_yn= ifelse(boy_prim_edu_barrier+ 
                               girl_prim_edu_barrier+
                               boy_second_edu_barrier+
                               girl_second_edu_barrier>0,1,0),
      wash_barrier_yn= ifelse(hh_water_problem=="yes",1,0),
      health_barrier_yn=ifelse(health_access=="yes",1,0),
      barrier_edu_wash_health=edu_barrier_yn+wash_barrier_yn+health_barrier_yn,
      si.ios.access_wash_education_health=ifelse(barrier_edu_wash_health>2,2,ifelse(barrier_edu_wash_health>0,1,ifelse(barrier_edu_wash_health==0,0,NA)))
    ) %>% select(instance_name,starts_with("si."))
  
  
  hh_level %>% colnames()
  dl<-list(hh_level,more_individual_to_HH,Individual_to_HH)
  all_indis<-Reduce(function(x, y) merge(x, y, all=TRUE), dl)}
