
library(devtools)
install_github("mabafaba/msni19")
install_github("mabafaba/hypegrammaR")
library(dplyr)
library(composr)
library(rlang)
library(ggplot2)
library(rgdal)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


hh<-read.csv("../02_Data/Refugee_MSNA_HH_Data.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))
indiv<-read.csv("../02_Data/Refugee_MSNA_Indiv_Data.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))
cont<-read.csv("../02_Data/Refugee_MSNA_Container_Data.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))
hh_water<-read.csv("../02_Data/MSNA_II_HH_WaterStats_24june2019.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))

shp_dir<-"../Severity_Pilot/input_public/01_shp"
ogrListLayers(shp_dir)
shp_file<- "190310_Outline_Rohingya_Refugee_Camp_A1"
file_path<-paste0(shp_dir,"/", shp_file)
cmp<-readOGR(dsn=shp_dir, layer = shp_file, stringsAsFactors = FALSE)
cmp$cmp_name<-cmp$New_Camp_N %>% unique() %>% tolower()
kbc<-c("camp 13", "camp 14", "camp 10",  "camp 6", "camp 18", 
       "camp 20", "camp 1e",  "camp 17", "camp 9", 
       "camp 8w", "camp 1w", "camp 15", "camp 5" , "camp 3", 
       "camp 16", "camp 2w", "camp 20 extension", "camp 11", "camp 4", 
       "camp 19",  "camp 7", "camp 4 extension", 
       "camp 8e", "camp 2e", "camp 12")

iso<-c("camp 21", "camp 22","camp 23")
st<-c("camp 24","camp 25","camp 26","camp 27", "nayapara rc")
cmp$region<-ifelse(cmp$cmp_name %in%  kbc, "Kutapalong",
       ifelse(cmp$cmp_name %in% iso, "Isolated Camps","Southern Teknaf"))



