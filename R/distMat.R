
#' @title distMat
#' @description Calcula la matriz de distancias a partir de coordenadas X y Y
#' @param coordMat data frame con las coordenadas x y y de los nodos
#'

distMat <- function(coordMat=NULL){
  
  dist <- matrix(data = NA,nrow =length(coordMat[,1]),ncol = length(coordMat[,1]))
  for(i in 1:length(coordMat[,1])){
    for(j in 1:length(coordMat[,1])){
      dist[i,j] <- euc.dist(x1 = coordMat[i,1],x2 =coordMat[j,1],y1 = coordMat[i,2],y2 =coordMat[j,2] )
    }
  }
  return(dist)
}
