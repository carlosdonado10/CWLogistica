#' Nodo Asignado
#' @description Evalúa la restricción de CW de que los nodos deben estar en rutas diferentes
#'
#' @param rutaStr Lista con las rutas calculadas por CW hasta el momento
#' @param nodos Vector con los nodos por los que se van a unir las rutas (edge[i,j]) 
#'
#' @return Boolen: True si se cumple, False de lo contrario

nodoAsignado <- function(rutaStr, nodos){
 
  #En qué ruta está
  for(i in 1:length(rutaStr)){
    if(nodos[1]%in%rutaStr[[i]]){
      pos1 <- i
    }
    if(nodos[2]%in%rutaStr[[i]]){
      pos2 <- i
    }
  }
  
  
   

    
  resp=pos1!=pos2
  return(resp)
}
