


#' Density of parititon
#'
#' @description Density of parititon
#' @usage density.clusters(graph, partition)
#' @param graph : an igraph graph
#' @param partition : a partition membership
#' @return a numeric value
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = DBLP10K
#' wt = ANCA(graph)
#' density.clusters(graph,wt)
#' @export

density.clusters <- function(graph, partition){
  if(!is.igraph(graph))
    stop("Should be applied on a igraph graph object")
  d = 0
  if(is.list(partition)){
    for(cluster in partition){
      d = d + ecount(induced_subgraph(graph,cluster))
    }
    return(d/ecount(graph))
  }
  else if(is.vector(partition)){
    density.clusters(graph,memb2groups(partition))
  }
  else
    stop("argument partition should be a list or a vector")

  #length(which(crossing(wt,graph)==FALSE))/ecount(graph  )
}
