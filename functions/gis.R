
#FUNCTIONS

str_standardize<- function(name_string){
  str_replace_all(name_string, "[[:punct:]]", " ") %>% tolower() %>% trimws()
}
str_standardize_cxb_camps<-function(x){
  str_standardize(x) %>% str_replace_all(c("extn"= "extension"))
}


#' TITLE
#'
#' description blabla
#'
#' @param data the dataset
#' @param polygon a spatial polygon data frame (spdf)
#' @param match_on_data column name as character string used to match the data to the geometries
#' @param ... further arguments passed to `FUN`
#' @details
#' @return
#' @export
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


#' @export
choropleth<- function(spatial_data, label_col=NULL, numeric_col,title_map= NULL){
  
  if(!("SpatialPolygonsDataFrame" %in% class(spatial_data))){stop("spatial_data must be an object of class 'SpatialPolygonsDataFrame'")}
  if(!is.null(label_col)){
    if(!(label_col %in% names(spatial_data))){stop(glue::glue("{label_col} is not a column name in spatial_data"))}
  }  

    if(!(numeric_col %in% names(spatial_data))){stop(glue::glue("{numeric_col} is not a column name in spatial_data"))}

  assertthat::assert_that(!is.null(spatial_data))
  assertthat::assert_that(!is.na(spatial_data))
  assertthat::assert_that(!is.null(numeric_col))  
  assertthat::assert_that(!is.na(numeric_col))
  assertthat::assert_that(assertthat::is.string(numeric_col))
  assertthat::assert_that(assertthat::is.string(label_col))
  
  if(!is.null(title_map)){
      assertthat::assert_that(assertthat::is.string(title_map))
  }
  
  label_list=list("sp.text", coordinates(spatial_data), as.character(spatial_data@data[,label_col]),col="black", cex=0.7,font=2)
  map1<-spplot(spatial_data,numeric_col, col.regions = colorRampPalette(c("grey", "beige", "red"))(10), main=title_map,
               sp.layout= list(label_list))
}








result_disaggregated_by_match_id_as_map<-function(result,assessment, color_scale_name = "Mean Severity Score"){
  severity_disaggregated<-result
  sum_stats_table<- severity_disaggregated$summary.statistic
  sum_stats<-data.frame(match_id= sum_stats_table$independent.var.value, mean=round(sum_stats_table$numbers,2),stringsAsFactors = FALSE)
  
  sum_stats$match_id<-str_standardize_cxb_camps(sum_stats$match_id)
  
  sf<-assessment_data_as_sf(sum_stats,assessment)
  
  
  sf <- sf %>%
    mutate(
      lon = purrr::map_dbl(geometry, ~ st_centroid(.x)[[1]]),
      lat = purrr::map_dbl(geometry, ~ st_centroid(.x)[[2]])
    )
  
  
  
  limits<-c(min(sf$mean,na.rm = T),max(sf$mean,na.rm = T))
  plots<-sf %>% split(sf[["region"]]) %>% purrr::map(function(x){
    plot <- ggplot(x)+
      geom_sf(aes(fill=mean),color = 'white')+
      theme_minimal()+
      # scale_fill_continuous(limits=limits, name= color_scale_name)+
      # scale_fill_brewer( palette="YlOrRd",limits=limits, name= color_scale_name )+
      # scale_fill_gradient(low = "#56B1F7", high = "#132B43",limits=limits, name= color_scale_name )+
      scale_fill_gradient(low = "beige", high = "red",limits=limits, name= color_scale_name )+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks =element_blank())+
      geom_text_repel(aes(x = lon, y = lat, label = match_id))
    print(plot)
  })
  
  
}


