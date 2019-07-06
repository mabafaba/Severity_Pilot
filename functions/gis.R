
library(devtools)
install_github("mabafaba/msni19")
install_github("mabafaba/hypegrammaR")
library(dplyr)
library(composr)
library(rlang)
library(ggplot2)
library(rgdal)


str_standardize<- function(name_string){
  str_replace_all(name_string, "[[:punct:]]", " ") %>% tolower() %>% trimws()
}

check_not_in< function(list one)

# setwd("GitHub")

getwd()
hh<-read.csv("Severity_Pilot/input/RefugeeMSNA_csv/Refugee_MSNA_HH.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))
# indiv<-read.csv("../02_Data/Refugee_MSNA_Indiv_Data.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))
# cont<-read.csv("../02_Data/Refugee_MSNA_Container_Data.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))
# hh_water<-read.csv("../02_Data/MSNA_II_HH_WaterStats_24june2019.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))

shp_dir<-"Severity_Pilot/input_public/01_shp"
ogrListLayers(shp_dir)
shp_file<- "190310_Outline_Rohingya_Refugee_Camp_A1"
file_path<-paste0(shp_dir,"/", shp_file)
cmp<-readOGR(dsn=shp_dir, layer = shp_file, stringsAsFactors = FALSE)

kbc<-c("camp 13", "camp 14", "camp 10",  "camp 6", "camp 18", 
       "camp 20", "camp 1e",  "camp 17", "camp 9", 
       "camp 8w", "camp 1w", "camp 15", "camp 5" , "camp 3", 
       "camp 16", "camp 2w", "camp 20 extension", "camp 11", "camp 4", 
       "camp 19",  "camp 7", "camp 4 extension", 
       "camp 8e", "camp 2e", "camp 12")

iso<-c("camp 21", "camp 22","camp 23")
st<-c("camp 24","camp 25","camp 26","camp 27", "nayapara rc") 


hh$camp_location_standardized<-hh$camp_location %>% str_standardize() %>% str_replace_all(c("extn"= "extension"))
cmp$camp_name_standardize<- cmp$New_Camp_N %>% str_standardize


cmp$region<-ifelse(cmp$camp_name_standardize %in%  kbc, "Kutapalong",
       ifelse(cmp$camp_name_standardize %in% iso, "Isolated Camps","Southern Teknaf"))

assessment$data$camp_location_standardize<-assessment$data$camp_location %>% str_standardize() %>% str_replace_all(c("extn"= "extension"))
assessment$severity
assessment$data %>% 
  group_by(camp_location_standardize) %>%
  summarize()



svyttest( INCOME ~ GENDER , subset( example.survey , RELOCATE == 1 ) )



# cmp$camp_name_matched<-str_replace_all(string = cmp$cmp_name, c(" "="_","extension"= "extn")) %>% tolower()
# cmp$camp_name_matched[ which(cmp$camp_name_matched %in% hh$camp_location==FALSE)]

hh$camp_location_standardized %in% hh$
camp_name_matched
unique(hh$camp_location)
library(stringr)
title_map<-"Distribution of Severity Scores"
 

 
  
  paste0(nrow(dat1)," tubewells located in", length(camps_mapped2), " camp(s) and ",nrow(fw_counts1)," grid cells") 

l1 = list("sp.text", coordinates(grid_roi), as.character(grid_roi@data$labels_simp),col="black", cex=0.7,font=2)
l2 = list("sp.text", coordinates(grid_roi), as.character(grid_roi@data$labels),col="black", cex=0.7,font=10)
l3 = list("sp.text", coordinates(grid_roi), as.character(grid_roi@data$labels_for_meeting_big),col="black", cex=0.7,font=2)

map1<-spplot(grid_roi,"fw_perc_max", col.regions = colorRampPalette(c("grey", "beige", "red"))(16), main=title_map,
             sp.layout= list(l1))
map1

map_export<-spplot(grid_roi,"fw_perc_max", col.regions = colorRampPalette(c("grey", "beige", "red"))(16), sp.layout=list(l1), main=title_map)

map_export2<-spplot(grid_roi,"fw_perc_max", col.regions = colorRampPalette(c("grey", "beige", "red"))(16), sp.layout=list(l3), main=title_map)



