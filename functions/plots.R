
weighted_ecdf<-function(x,weights){
  df<-tibble(x=x,w=weights)
  df <- df[order(df$x), ]  # Won't change anything since it was created sorted
  df<- df %>% filter(!is.na(x)&!is.na(w))
  cum.pct <- cumsum(x * weights) / sum(x * weights,na.rm = T)
  df$cum.pct <- with(df, cumsum(x * w) / sum(x * w))
  df
}


faceted_density_plot<-function(data, x_variable_name, facet_by_variable_name){

  if(x_variable_name %not a column in% data){
    stop(glue("{x_variable_name} not a column name in the data"))
  }
  
  if(facet_by_variable_name %not a column in% data){
    stop(glue("{facet_by_variable_name} not a column name in the data"))
  }
  
  ggplot(data)+
    geom_bar(aes_string(x=x_variable_name))+
    theme_minimal()+
    facet_grid(rows = vars(data[[facet_by_variable_name]]))
}




