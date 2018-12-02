#' @title matrizAhorros
#' @description Calcula la matriz de ahorros a partir de la matriz de distancias
#' @param distMat Matriz de distancias. Distancia del nodo i al nodo j. Ver distMat
#'
#' @return Matriz de Ahorros

matrizAhorros <- function(distMat=NULL){
  Ahorros <- matrix(data = NA,nrow =length(distMat[,1]),ncol = length(distMat[,1]))
  for(i in 1:length(distMat[,1])){
    for(j in 1:length(distMat[,1])){
      if(i!=j){
        Ahorros[i,j]=dist[i,1]+dist[j,1]-dist[i,j]
      }else{
        Ahorros[i,j]=0
      }
    }
  }
  Ahorros <- Ahorros[-1,]
  Ahorros <- Ahorros[,-1]#Meter en la funcion
  return(Ahorros)
}
