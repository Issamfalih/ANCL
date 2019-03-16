
#' CSM
#'
#' @description CSM
#' @usage
#' @param graph : an igraph graph
#' @return A igraph graph with attribute vertices
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = Polblogs
#' wt = CSM (graph )
#' @export




CSM <- function(graph, alpha = 0.5){

  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")

  attr = data.frame(row.names = V(graph)$name)
  for(attrName in setdiff(vertex_attr_names(graph),c("name","id")))
        attr = cbind(attr,attrName = get.vertex.attribute(graph,name = attrName))

  VerticesNames = V(graph)$name

  CSIM <- matrix(1,nrow = vcount(graph),ncol=vcount(graph))
  colnames(CSIM) <- VerticesNames
  rownames(CSIM) <-VerticesNames

  for(nodei in VerticesNames ){
    cat(paste0(nodei,"."))
    for(nodej in setdiff(VerticesNames,nodei)){
      struct <- 1
      context <- 1
      path <- as.vector(get.shortest.paths(graph,nodei,nodej)$vpath[[1]])
      if(length(path)!=0){
        for(i in 1:length(path)){
          struct <- struct  * (1/degree(graph,path[i]))
          context <- context * (matchingCoefficient(attr[path[i],],attr[path[i+1],]))
        }
        CSIM[nodei,nodej] <- (alpha*struct) + ((1-alpha)*context)
        CSIM[nodej,nodei] <- CSIM[nodei,nodej]

      }
    }
  }

  dist = 1/CSIM

  for(nodei in VerticesNames ){
    cat(paste0(nodei,"."))
    for(nodej in setdiff(VerticesNames,nodei)){
      path <- as.vector(get.shortest.paths(graph,nodei,nodej)$vpath[[1]])
      if(length(path)==0)
        dist[nodei,nodej] = Inf
      else if(length(path)>2)
        dist[nodei,nodej] = do.call(prod,CSIM[nodei,path])
      dist[nodei,nodej] =dist[nodej,nodei]
    }
    dist[nodei,nodei] <- 0
  }
  return(dist)
}


