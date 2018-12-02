#' TPrimeroUltimo
#'@description Resulelve si un par de nodos cumplen la restriccion de CW. Uno de ellos debe estar al inicio de alguna la ruta y el otro al final
#' @param nodos Vector con los nodos por los que se van a unir las rutas (edge[i,j]) 
#' @param rutaStr Lista con las rutas calculadas por CW hasta el momento

primeroUltimo <- function(nodos=NULL,rutaStr=NULL){
  primero <- 0
  #En qué ruta está
  for(i in 1:length(rutaStr)){
    if(nodos[1]%in%rutaStr[[i]]){
      pos1 <- i
    }
    if(nodos[2]%in%rutaStr[[i]]){
      pos2 <- i
    }
  }
  
  a <- F
  if(which(rutaStr[[pos1]]==nodos[1])==1 & which(rutaStr[[pos2]]==nodos[2])==length(rutaStr[[pos2]])){
    a <- T
    primero <- 1
  }
  if(which(rutaStr[[pos2]]==nodos[2])==1 & which(rutaStr[[pos1]]==nodos[1])==length(rutaStr[[pos1]])){
    a <- T
    primero <- 2
  }
  
  structure(list(cumple=a,primero=primero))
}
