#' Pegar Ruta
#'@description  Function que pega las rutas una vez se evaluó la factibilidad par ala heurística de Clarke-Wright. Ver CWCVRP
#' @param nodos vector con los nodos por los cuales se uniran las rutas (edge[i,j]). Nota:length(nodos)==2
#' @param rutaStr Lista con las rutas hasta el momento desarrolladas por la heuristica CW
#' @param noms (optional)Vector que contiene los nombres de los clientes

pegarRuta <- function(nodos=NULL,rutaStr=NULL,noms=NULL){
  
  for(i in 1:length(rutaStr)){
    if(nodos[1]%in%rutaStr[[i]]){
      pos1 <- i
    }
    if(nodos[2]%in%rutaStr[[i]]){
      pos2 <- i
    }
  }
  
  primero <- primeroUltimo(nodos = nodos,rutaStr = rutaStr)$primero
  ultimo <- which(primero!=c(1,2))
  
  rutaStr[[c(pos1,pos2 )[primero]]] <- c(rutaStr[[c(pos1,pos2 )[primero]]],rutaStr[[c(pos1,pos2 )[ultimo]]])
  rutaStr[[c(pos1,pos2 )[ultimo]]] <- NULL
  
  structure(list(rutaStr=rutaStr,eliminado=c(pos1,pos2 )[ultimo],asignado=c(pos1,pos2 )[primero]))
  

  
  
}
