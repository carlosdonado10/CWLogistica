#' @title vectorAhorros
#' @description Convierte la matriz de ahorros en el formato deseado [i,j,ahorro]
#' @param ahorros Matriz de ahorros calculada con la función matrizAhorros
#' @param noms Vector con las nombres de los clientes. Mismo orden que los nodos de la matriz
#'
#' @return data frame con [nodo i, nodo j, ahorro]

vectorAhorros <- function(ahorros=NULL,noms=LETTERS[1:dim(ahorros)[1]]){
  if(is.null(ahorros))stop("Los ahorros no pueden estar vacíos")
  df <- NULL
  df <- rep(noms,dim(ahorros)[1])
  a <- NULL
  for(i in 1:length(noms)){
      a <- c(a,rep(noms[i],dim(ahorros)[1]))
  } 
  df <- cbind.data.frame(df,as.character(a),stringsAsFactors=F)
  df <- cbind.data.frame(df,as.vector(t(ahorros)),stringsAsFactors=F)
  colnames(df) <- c("i","j","Ahorro")
   
  df <- df[order(df[,3],decreasing = T),]
  
  
  return(df)
  
  
}
