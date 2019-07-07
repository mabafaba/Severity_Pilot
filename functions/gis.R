
library(devtools)
install_github("mabafaba/msni19")
install_github("mabafaba/hypegrammaR")
library(dplyr)
library(composr)
library(rlang)
library(ggplot2)
library(rgdal)
library(sp)

#FUNCTIONS

str_standardize<- function(name_string){
  str_replace_all(name_string, "[[:punct:]]", " ") %>% tolower() %>% trimws()
}
#' @param ... further arguments passed to `FUN`
aggregate_to_spatial_bound<- function(data, polygon, match_data_on, match_shape_on,numeric_column,FUN=mean, ...){
  poly<-polygon
  str_standardize_cxb_camps<-function(x){
    str_standardize(x) %>% str_replace_all(c("extn"= "extension"))
  }
  name_of_stat<-paste0("mean_", numeric_column)
  data$numeric_col<-data[,numeric_column]
  data$camp_name_standardized<-data[,match_data_on] %>% str_standardize_cxb_camps()
  poly@data$camp_name_standardized<- poly@data[,match_shape_on] %>% str_standardize_cxb_camps()
  summary_statistic<-data %>% group_by(camp_name_standardized) %>% 
    summarise( !!name_of_stat:=FUN(numeric_col, ...))
  poly_with_data<-sp::merge(poly, summary_statistic, all.x=TRUE, all.y=FALSE)
  poly_with_data<-poly_with_data[!is.na(poly_with_data$camp_name_standardized),]
  return(poly_with_data)
}

choropleth<- function(spatial_data, label_col=NULL, numeric_col,title_map= NULL){
  label_list=list("sp.text", coordinates(spatial_data), as.character(spatial_data@data[,label_col]),col="black", cex=0.7,font=2)
  map1<-spplot(spatial_data,numeric_col, col.regions = colorRampPalette(c("grey", "beige", "red"))(10), main=title_map,
               sp.layout= list(label_list))
}


# setwd("C:\\Users\\Felipe Ponce\\Documents\\GitHub")
# setwd("GitHub")
getwd()
hh<-read.csv("input/RefugeeMSNA_csv/Refugee_MSNA_HH.csv", stringsAsFactors = FALSE, na.strings = c(""," ", NA))


shp_dir<-"input_public/01_shp"
ogrListLayers(shp_dir)
layer_name<- "190310_Outline_Rohingya_Refugee_Camp_A1"
shp_directory<-paste0(shp_dir,"/", shp_file)

cmp<-readOGR(dsn=shp_dir, layer = layer_name, stringsAsFactors = FALSE)





hh %>% select_if(is.numeric) %>% colnames()
cmp_with_data <- aggregate_to_spatial_bound(
  hh,
  polygon = cmp,
  match_data_on = "camp_location",
  match_shape_on = "New_Camp_N",
  numeric_column = "HH_size",
  FUN = sum,
  na.rm = FALSE
)


map1<-choropleth(title_map= "Map of HH Size",spatial_data = cmp_with_data, label_col = "New_Camp_N",numeric_col =  "mean_HH_size")






windows();map1
title_map<-"Distribution of Severity Scores"
l1 = list("sp.text", coordinates(cmp_with_data), as.character(cmp_with_data@data$mean_numeric_column),col="black", cex=0.7,font=2)
map1<-spplot(cmp_with_data,"mean_numeric_column", col.regions = colorRampPalette(c("grey", "beige", "red"))(16), main=title_map,
             sp.layout= list(l1))





windows();map1
l2 = list("sp.text", coordinates(grid_roi), as.character(grid_roi@data$labels),col="black", cex=0.7,font=10)
l3 = list("sp.text", coordinates(grid_roi), as.character(grid_roi@data$labels_for_meeting_big),col="black", cex=0.7,font=2)

map1

map_export<-spplot(grid_roi,"fw_perc_max", col.regions = colorRampPalette(c("grey", "beige", "red"))(16), sp.layout=list(l1), main=title_map)

map_export2<-spplot(grid_roi,"fw_perc_max", col.regions = colorRampPalette(c("grey", "beige", "red"))(16), sp.layout=list(l3), main=title_map)

aggregate_to_boundary_sp_merge()



gis1<-function(data, shape_dir, shp_lyr, numeric_column){
  numeric_col<-data[,numeric_column]
  cmp<-readOGR(dsn=shape_dir,layer=shp_lyr,stringsAsFactors = FALSE)
  data$camp_name_standardized<-data$camp_location %>% str_standardize() %>% str_replace_all(c("extn"= "extension"))
  cmp$camp_name_standardized<- cmp$New_Camp_N %>% str_standardize()
  # kutapalong_mega_camps<-c("camp 13", "camp 14", "camp 10",  "camp 6", "camp 18", 
  #        "camp 20", "camp 1e",  "camp 17", "camp 9", 
  #        "camp 8w", "camp 1w", "camp 15", "camp 5" , "camp 3", 
  #        "camp 16", "camp 2w", "camp 20 extension", "camp 11", "camp 4", 
  #        "camp 19",  "camp 7", "camp 4 extension", 
  #        "camp 8e", "camp 2e", "camp 12")
  # isolated_camps<-c("camp 21", "camp 22","camp 23")
  # southern_teknaf<-c("camp 24","camp 25","camp 26","camp 27", "nayapara rc") 
  # cmp$region<-ifelse(cmp$camp_name_standardized %in%  kutapalong_mega_camps, "Kutapalong",
  #                    ifelse(cmp$camp_name_standardized %in% isolated_camps, "Isolated Camps","Southern Teknaf"))
  summary_statistic<-data %>% group_by(camp_name_standardized) %>% 
    summarise(mean_numeric_column=mean(numeric_col,na.rm=TRUE))
  cmp_with_data<-sp::merge(cmp, summary_statistic, all.x=TRUE, all.y=FALSE)
  cmp_with_data<-cmp_with_data[!is.na(cmp_with_data$camp_name_standardized),]
  # map1<-spplot(grid_roi,"fw_perc_max", col.regions = colorRampPalette(c("grey", "beige", "red"))(16), main="asdf",
  #              sp.layout= list(l1))
  standardized_data_and_shp<- list(cmp, data,summary_statistic,cmp_with_data) 


  return(standardized_data_and_shp)
  
  }
?merge
sp::merge
hh %>% select_if(is.numeric) %>% colnames()
asdf<-gis1(hh,
           shp_dir,
           layer_name,
           "wat_containers_num")
asdf[[3]]
nrow(cmp)
nrow()
asdf

asdf[[2]] %>% group_by(camp_name_standardize) %>% 
  summarise(mean_numeric_column=mean(wat_containers_num,na.rm=TRUE))

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



