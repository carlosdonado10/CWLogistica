#' CalcularCapacidad
#'@description Evalua la restricción de capacidad de la heurística CW
#' @param rutaStr Lista con las rutas calculadas por CW hasta el momento
#' @param nodos Vector con los nodos por los que se van a unir las rutas (edge[i,j]) 
#' @param demandas Vector con las demandas de los clientes. Mismo orden que los nombres
#' @param maxCap Capacidad máxima del vehículo
#'
#' @return cupo: Boolean. TRUE si la restricción de capacidad de cumple, 0 dlc
#' @return capacidad: Capacidad de la ruta luego de unir.


calcularCapacidad <- function(rutaStr=NULL,nodos=NULL,demandas=NULL,maxCap=NULL){
  if(is.null(rutaStr))stop("No hay lista de rutas")
  if(is.null(nodos))stop("No hay nodos para revisar capacidad")
  
  for(i in 1:length(rutaStr)){
    if(nodos[1]%in%rutaStr[[i]]){
      pos1 <- i
    }
    if(nodos[2]%in%rutaStr[[i]]){
      pos2 <- i
    }
  }
  
  a <- 0 
  for(i in 1:length(rutaStr[[pos1]])){
    a <- a+demandas$dem[which(demandas$clients==rutaStr[[pos1]][i])]
  }
  
  
  for(i in 1:length(rutaStr[[pos2]])){
    a <- a+demandas$dem[which(demandas$clients==rutaStr[[pos2]][i])]
  }
  
  if(maxCap>=a){
    resp <- TRUE
  }else{
    resp <- FALSE
  }
  
  structure(list(cupo=resp,capacidad=a))
}
