#' Clarke Wright Capacity Constrained Vehicle Routing Problem
#'
#' @param maxCap : Capacidad máxima del vehiculo
#' @param depo: Data frame que incluya las siguientes variables: id del nodo,Coordenada x, Coordenada y, Demanda. Solamente utilizará la primera fila (1 depósito).
#' @param client: Data frame que incluya las siguientes variables: id del nodo,Coordenada x, Coordenada y, Demanda
#'
#' @return Devuelve una lista de rutas asignadas mediante la heurística CW 
#' 
#' @export
#'
#' @examples dem <- c(10,15,18,17,3,5,9,4,6)
#' @examples dist <- matrix(c(0,12,11,7,10,10,9,8,6,12,12,0,8,5,9,12,14,16,17,22,11,8,0,9,15,17,8,18,14,22,7,5,9,0,7,9,11,12,12,17,10,9,15,7,0,3,17,7,15,18,10,12,17,9,3,0,18,6,15,15,9,14,8,11,17,18,0,16,8,16,8,16,18,12,7,6,16,0,11,11,6,17,14,12,15,15,8,11,0,10,12,22,22,17,18,15,16,11,10,0),ncol=10,nrow=10)
#' @examples CWVRP(maxCap = 40,dist = dist,clients = 1:9,dem = dem)
CWVRP <- function(maxCap=NULL,dist=NULL,clients=LETTERS[1:(dim(dist)[1]-1)],dem=NULL){
  if(is.null(dist))stop("Debe introducir la matriz de distancias")
  if(is.null(dem))stop("Debe introducir la matriz de demandas")
  if(is.null(maxCap))stop("Debe introducir la capacidad del vehículo")
  
  ##Revisar tamaño de demandas
  # dem <- c(10,15,18,17,3,5,9,4,6)
  # dist <- matrix(c(0,12,11,7,10,10,9,8,6,12,12,0,8,5,9,12,14,16,17,22,11,8,0,9,15,17,8,18,14,22,7,5,9,0,7,9,11,12,12,17,10,9,15,7,0,3,17,7,15,18,10,12,17,9,3,0,18,6,15,15,9,14,8,11,17,18,0,16,8,16,8,16,18,12,7,6,16,0,11,11,6,17,14,12,15,15,8,11,0,10,12,22,22,17,18,15,16,11,10,0),ncol=10,nrow=10)
  dem <- cbind.data.frame(clients,dem,stringsAsFactors=F)
  Ahorros <- matrizAhorros(distMat = dist)
  Ahorros <- vectorAhorros(ahorros = Ahorros,noms=clients)
  Ahorros <- Ahorros[seq(1, length(Ahorros$i)-1, 2),]

  #Algoritmo Clarke Wright----
  rutaStr <- list()
  algStr <- NULL
  
  #Crear todas las rutas triviales
  for(i in 1:length(clients)){
    rutaStr[[i]] <- clients[i]
  }
  
  
  for(i in 1:length(Ahorros$Ahorro)){
    if(max(Ahorros$Ahorro)==0)break
    
    
    nodos <- c(Ahorros$i[1],Ahorros$j[1])
    capacidad <- calcularCapacidad(rutaStr = rutaStr,nodos = nodos,demandas = dem,maxCap = maxCap)
    #Condiciones para pegar rutas
    if(
      #Los Nodos se encuentran en ciclos separados
      nodoAsignado(rutaStr = rutaStr,nodos=nodos)
      &
      #Capacidad
      capacidad$cupo
      &
      #Los nodos que se van a agregar son el primero y último
      primeroUltimo(nodos = nodos,rutaStr = rutaStr)$cumple
    ){
      
      pegar <- pegarRuta(nodos = nodos,rutaStr = rutaStr,noms=clients)
      rutaStr <- pegar$rutaStr
    }
    Ahorros <- Ahorros[-1,]
    rutaStr
  }
  
  return(rutaStr)
}
