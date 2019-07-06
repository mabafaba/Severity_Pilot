install.packages("prcomp")
library(pra)

assessment$data$hhh_female<-    (assessment$data$HH_head_gender=="female") | (assessment$data$respondent_gender =="female") %>% as.numeric




prcomp_input<-data.frame(assessment$data$msni %>% as.numeric,
                         assessment$data$hhh_female,
                         assessment$data$hh_size %>% as.numeric)



pca<- prcomp(prcomp_input,scale. = T,center = T) 


lm(as.numeric(assessment$data$msni)~log(assessment$data$hh_size)) %>% plot

dmsni <- data %>% 
  select(-starts_with("X_")) %>%
  select_if(is.numeric) %>%
  .[complete.cases(.),] %>%
  select_if(function(x){length(which(is.na(x)))/length(x)<0.2}) %>%
  select_if((function(x){stats::var(as.numeric(x))>=0.001})) %>%
  select_if(function(x){length(unique(x))>2})
pr <- data %>% 
  select(-msni,-starts_with("X_")) %>%
  select_if(is.numeric) %>%
  .[complete.cases(.),] %>%
  select_if(function(x){length(which(is.na(x)))/length(x)<0.2}) %>%
  select_if((function(x){stats::var(as.numeric(x))>=0.001})) %>%
  select_if(function(x){length(unique(x))>2}) 
pca1<-pr %>%  prcomp(scale. =T, center = T)

dmsni$pc1<-pca1$x[,1]
dmsni$pc2<-pca1$x[,2]
ggplot(dmsni,aes(x=pc1,y=msni))+geom_jitter(width=0,height=0.2)

ggplot(dmsni,aes(x=pc2,y=msni))+geom_point(width=0,height=0.2)





plot()
pr <- data %>% 
  select(-msni,-starts_with("X_")) %>%
  select_if(is.numeric) %>%
  .[complete.cases(.),] %>% 
  select_if(function(x){length(which(is.na(x)))/length(x)<0.2}) %>%
  select_if((function(x){stats::var(as.numeric(x))>=0.001})) %>%
  select_if(function(x){length(unique(x))>2}) %>% 
  prcomp(scale. =T, center = T)
windows()
  pr %>% biplot
  pr %>% plot
  pr$rotation
  library(tidyr)
  pr$x
  ggplot()+geom_bar(aes(y = PC1),stat = 'identity')
  
data$pc1<-pr$x[,1]
  
  
  pr$rotation %>% as.data.frame%>% str
  
ggplot(data)+geom_bar(aes(x=assessment.data.hh_size.....as.numeric,
                                  y=(assessment.data.msni.....as.numeric)))







assessment$data$hhh_female[assessment$data$HH_head_gender == "female"]



