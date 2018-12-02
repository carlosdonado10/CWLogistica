#' eucDist
#' @description  Calcula las distancias euclidianas a partir de las coordenadas X y Y
#' @param x1 
#' @param y1 
#' @param x2 
#' @param y2 

euc.dist <- function(x1, y1,x2,y2){
  ((x1 - x2) ^ 2+(y1-y2)^2)^0.5
  
}