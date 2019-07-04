percent_na<-function(x){(length(which(is.na(x)))/length(x)) %>% multiply_by(100) %>% round(3) %>% paste("%")}


`%not a column in%` <- function(x,y){
 !( x %in% colnames(y) )
}
